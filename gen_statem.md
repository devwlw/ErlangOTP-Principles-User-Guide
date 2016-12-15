### 4 gen_statem行为

本章应该与STDLIB手册中的[gen_statem](http://erlang.org/doc/man/gen_statem.html)一节一起阅读,包含了所有的接口函数与回调函数的定义。

<h3 id="1"> 4.1 事件驱动的状态机</h3>

已建立的自动机理论不太关心状态转换时如何触发的，但是假设输出是输入（和状态）函数并且它们是某种值。
对于一个事件驱动的状态机，输入事件触发一个状态转换并且输出是在状态转换期间执行的动作。与有限状态机的数学模型近似，可以描述为一组关系集合，就像下面这种形式：
```
State(S) x Event(E) -> Actions(A), State(S')
```

他们的关系可以解释成：若我们正处于**S**状态并且触发了事件**E**，我们要执行**A**操作并且转换状态到**S'**。注意**S'**可能与**S**相等。

当**A**和**S'**只依赖于**S**和**E**时，这种类型的状态机我们叫做[Mealy Machine](https://en.wikipedia.org/wiki/Mealy_machine)。

就像大部分的**gen_**行为，**gen_statem**保存除服务器状态之外的数据。正因为这样，对状态（假设有足够的虚拟机内存）或不同输入的数量没有限制，由此行为实现的状态机实际上是[图灵完备](http://stackoverflow.com/questions/7284/what-is-turing-complete)。但是感觉更像是事件驱动的Mealy机。

#### 4.2 回调模式

**gen_statem**行为支持两种回调模式：

- 在[state_functions](http://erlang.org/doc/man/gen_statem.html#type-callback_mode)模型中，状态转换规则被写为一些Erlang函数，符合如下这种约定：
```
StateName(EventType, EventContent, Data) ->
    .. code for actions here ...
    {next_state, NewStateName, NewData}.
```
- 在[handle_event_function](http://erlang.org/doc/man/gen_statem.html#type-callback_mode)模式中，只为所有的状态转换规则提供了一个Elang函数：
```
handle_event(EventType, EventContent, State, Data) ->
    .. code for actions here ...
    {next_state, NewState, NewData}
```

Both these modes allow other return tuples; see [Module:StateName/3](http://erlang.org/doc/man/gen_statem.html#Module:StateName-3) in the **gen_statem** manual page. These other return tuples can, for example, stop the machine, execute state transition actions on the machine engine itself, and send replies.

##### 选择回调模式

两种[回调模式](http://erlang.org/doc/design_principles/statem.html#callback_modes)提供了不同的可能性与限制，但记住它们只有一个目标：处理事件与状态的所有可能组合。

这是可以做到的，例如关注当时的一种状态并且确保对于每种状态所有的事件都被处理了，或者关注当时的一个事件并且确保它在每一个状态中被处理。当然也可以混合使用

使用**state_functions**时，只能使用原子（atom-only）状态，**gen_statem**引擎分支取决于你使用的状态名。这鼓励回调模块收集所有事件动作的实现，特别是代码中相同位置中的一种状态，因此专注于当时的一种状态。

当你有一个常规状态图时，这个模式很适合。像本章中提到的那些一样，描述属于状态的所有事件和动作，并且每个状态都有唯一的名称。

使用**handle_event_function**时，可以随意使用混合策略，所有的时间和状态都在一个相同的回调函数里处理。

当你想关注当时的一种事件或者状态时，这种模式同样能很好的工作，但[Module:handle_event/4](http://erlang.org/doc/man/gen_statem.html#Module:handle_event-4)会快速增长而无法分支到辅助函数。 

这种模式允许使用非原子的状态，例如复杂或者分层状态。对于协议两端的客户端和服务端的状态图有很大程度上的相似，你可以使用 **{StateName,server}**或者**{StateName,client}**状态中的**StateName**来决定在该代码里哪处处理该状态的大部分事件。元组中的第二个参数用来选择是否处理特殊的客户端或服务端事件。

### 4.3  状态输入调用

每当状态改变时，**gen_statem**行为可以不管回调模式而自动调用具有特定参数的[回调函数](http://erlang.org/doc/man/gen_statem.html#type-state_enter)，所以你可以在其余的状态转换规则附近写入状态条目动作。通常是这样：

```
StateName(enter, _OldState, Data) ->
    ... code for state entry actions here ...
    {keep_state, NewData};
StateName(EventType, EventContent, Data) ->
    ... code for actions here ...
    {next_state, NewStateName, NewData}.
```
根据所你指定的状态机，这是一个非常有用的功能，但是却会强制你在所有的状态中处理状态输入调用。另见[状态输入操作](#state_in)一章

### 4.4 动作

在第一节[事件驱动状态机](#1)中，动作被提及为一般状态机模型的一部分。在返回给**gen_statem**引擎之前，回调模块**gen_statem**在一个事件处理回调函数中被执行的同时实现了这些一般动作。

在回调函数返回后，有许多特定的状态转换动作来供回调函数来操作**gen_statem**引擎使用。它们为[回调函数](http://erlang.org/doc/man/gen_statem.html#Module:StateName-3)返回的[元组](http://erlang.org/doc/man/gen_statem.html#type-state_callback_result)中的一个[动作](http://erlang.org/doc/man/gen_statem.html#type-action)列表。这些状态转换动作可以影响到**gen_statem**本身并且可以做下面这些事：

* [延迟](http://erlang.org/doc/man/gen_statem.html#type-postpone)当前事件，见[Postponing Events](http://erlang.org/doc/design_principles/statem.html#Postponing%20Events)
* [休眠](http://erlang.org/doc/man/gen_statem.html#type-hibernate)**gen_statem**，见[Hibernation](#hibernation)
* 开启[超时状态](http://erlang.org/doc/man/gen_statem.html#type-state_timeout)，见[State Time-Outs](#state_timeout)
* 启动[超时事件](http://erlang.org/doc/man/gen_statem.html#type-event_timeout)，见[Event Time-Outs](#eevent_timeout)
* [回复](http://erlang.org/doc/man/gen_statem.html#type-reply_action)调用者，见[All State Events](#all_state_event)
* 生成要处理的[下一个事件](http://erlang.org/doc/man/gen_statem.html#type-action)，见[Self-Generated Events]($self_gen_event)

详情请查看手册[gen_statem(3)](http://erlang.org/doc/man/gen_statem.html#type-action)一章。举个例子，你可以回复多个调用者并且生成多个next events来处理

### 4.5 事件类型

事件分类成多种不停的[事件类型](http://erlang.org/doc/man/gen_statem.html#type-event_type)。所有类型的事件都在相同的回调函数中处理，对于给定的状态，函数将**EventType**与**EventContent**作为参数。
下面是一个完整的事件类型列表以及他们的来处：

*cast* &nbsp; 由[gen_statem:cast](http://erlang.org/doc/man/gen_statem.html#cast-2)生成

*{call,From}*  &nbsp;  由[gen_statem:call](http://erlang.org/doc/man/gen_statem.html#call-2)生成，其中**From**是通过状态转换操作**{reply，From，Msg}**或通过调用[gen_statem:reply](http://erlang.org/doc/man/gen_statem.html#reply-1)应答时使用的回复地址。

*info*  &nbsp;  由发送到**gen_statem**进程的任意常规进程消息生成。

*state_timeout*  &nbsp;  由状态转换动作[{state_timeout,Time,EventContent}](http://erlang.org/doc/man/gen_statem.html#type-state_timeout)，状态定时器超时生成。

*timeout*  =>  &nbsp;  由状态转换动作[{timeout,Time,EventContent}](http://erlang.org/doc/man/gen_statem.html#type-event_timeout)(或者**Time**)，事件定时器超时生成。

*internal*  =>  &nbsp;  由状态转换动作**{next_event,internal,EventContent}**生成。

以上所有事件类型同样可以使用**{next_event,EventType,EventContent}**生成


### 4.6 例子

此实例等效于[gen_fsm行为](<gen_fsm.md>)中的例子。后面部分的例子中使用了**gen_statem**中的额外功能**gen_fsm**并没有这些功能，章节末尾提供了一个包含所有额外功能的例子。

一个密码锁的门可以被视作为一个装提及。开始门是关着的，当有人按下按钮就会生成一个事件，根据之前按下的按钮，到目前为止的顺序可能是正确的，不完整的或者错误的。若正确，门将会解锁10秒（10000微秒）。若不完整则等待按下其它键，若错误则重来等一个新的按钮序列。

![code lock state diagram](http://erlang.org/doc/design_principles/code_lock.png)
##### 图4.1 密码锁状态图

密码锁状态机可以由**gen_statem**与下面的回调模块实现：

```
-module(code_lock).
-behaviour(gen_statem).
-define(NAME, code_lock).

-export([start_link/1]).
-export([button/1]).
-export([init/1,callback_mode/0,terminate/3,code_change/4]).
-export([locked/3,open/3]).

start_link(Code) ->
    gen_statem:start_link({local,?NAME}, ?MODULE, Code, []).

button(Digit) ->
    gen_statem:cast(?NAME, {button,Digit}).

init(Code) ->
    do_lock(),
    Data = #{code => Code, remaining => Code},
    {ok,locked,Data}.

callback_mode() ->
    state_functions.

locked(
  cast, {button,Digit},
  #{code := Code, remaining := Remaining} = Data) ->
    case Remaining of
        [Digit] ->
	    do_unlock(),
            {next_state,open,Data#{remaining := Code},10000};
        [Digit|Rest] -> % Incomplete
            {next_state,locked,Data#{remaining := Rest}};
        _Wrong ->
            {next_state,locked,Data#{remaining := Code}}
    end.

open(timeout, _,  Data) ->
    do_lock(),
    {next_state,locked,Data};
open(cast, {button,_}, Data) ->
    do_lock(),
    {next_state,locked,Data}.

do_lock() ->
    io:format("Lock~n", []).
do_unlock() ->
    io:format("Unlock~n", []).

terminate(_Reason, State, _Data) ->
    State =/= locked andalso do_lock(),
    ok.
code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.
```
代码的讲解在下一节。

### 4.7  启动gen_statem

在前几节的例子中，**gen_statem**通过调用**code_lock:start_link(Code)**来启动：

```
start_link(Code) ->
    gen_statem:start_link({local,?NAME}, ?MODULE, Code, []).
```

**start_link**调用了[gen_statem:start_link/4](http://erlang.org/doc/man/gen_statem.html#start_link-4)函数，它能分裂并链接到一个新的**gen_statem**进程。

- 第一个参数 **{local,?NAME}**指定了名称。在本例中**gen_statem**通过 **?NAME**宏在局部注册为了**code_lock**。 

若忽略了名称，**gen_statem**则不会被注册，必须使用其pid。名称同样可以通过**{global,Name}**指定，然后**gen_statem**使用Kernel中的[gloal:register_name/2](http://erlang.org/doc/man/global.html#register_name-2)注册。

- 第二个参数**?MODULE**是回调模块的名称。也就是说，此模块在回调函数所在的位置。

接口函数**start_link/1**和**button/1**与回调函数**init/1**和**open/3**位于相同的模块中。客户端代码与服务端代码在同一个模块中通常是一种良好的编程实践。

- 第三个参数**Code**是一个数字列表，是传递个回调函数**init/1**的正确解锁密码。

- 第四个参数**[]**是一个设置列表，可以在[gen_statem:start_link/3](http://erlang.org/doc/man/gen_statem.html#start_link-3)查看可用的设置。

若名称注册成功，新的**gen_statem**进程调用回调函数**code_lock:init(Code)**。这个函数返回**{ok,State,Data}**，其中**State**是**gen_statem**的初始状态，在本例中为**locked**，假设门在刚开始就是锁着的。**Data**是**gen_statem**的内部服务器数据。这里的服务器数据是保存了正确按钮序列的一个[map](http://erlang.org/doc/man/maps.html)（与刚开始的代码一样）。

```
init(Code) ->
    do_lock(),
    Data = #{code => Code, remaining => Code},
    {ok,locked,Data}.
```

[gen_statem:start_link](http://erlang.org/doc/man/gen_statem.html#start_link-3)是同步的，当**gen_statem**没有初始化和准备好接收事件时,它是不会返回的
如果**gen_statem**是监督树的一部分,即由一个supervisor启动,那么必须使用[gen_statem:start_link](http://erlang.org/doc/man/gen_statem.html#start_link-3)。另外有一个**gen_statem:start**函数,可以启动一个独立的服务器,也就是说一个**gen_statem**不是监督树的一部分。

```
callback_mode() ->
    state_functions.
```

[ Module:callback_mode/0](http://erlang.org/doc/man/gen_statem.html#Module:callback_mode-0)函数为回调模块选择[CallbackMode](http://erlang.org/doc/design_principles/statem.html#callback_modes)，在这个例子中是[state_functions](http://erlang.org/doc/man/gen_statem.html#type-callback_mode)。也就是说，每一种状态都有其自己的处理函数。

### 4.8 事件处理

通知密码锁的按钮事件的函数由[gen_statem:cast/2](http://erlang.org/doc/man/gen_statem.html#cast-2)实现：
```
button(Digit) ->
    gen_statem:cast(?NAME, {button,Digit}).
```

第一个参数是**gen_statem**的名称，必须使用此名称来启动它。所以我们在启动它的时候使用**?NAME**宏。**{button,Digit}**为事件内容。

事件被转换成一条消息并且发送到**gen_statem**。当事件到达时,**gen_statem**调用**StateName(cast, Event, Data)**,返回一个元祖**{next_state,NewStateName,NewData}**。**StateName**是当前状态的名称，**NewStateName**是进入下一个状态名称，**NewData**是一个新的**gen_statem**服务器数据。

```
locked(
  cast, {button,Digit},
  #{code := Code, remaining := Remaining} = Data) ->
    case Remaining of
        [Digit] -> % Complete
	    do_unlock(),
            {next_state,open,Data#{remaining := Code},10000};
        [Digit|Rest] -> % Incomplete
            {next_state,locked,Data#{remaining := Rest}};
        [_|_] -> % Wrong
            {next_state,locked,Data#{remaining := Code}}
    end.

open(timeout, _, Data) ->
    do_lock(),
    {next_state,locked,Data};
open(cast, {button,_}, Data) ->
    do_lock(),
    {next_state,locked,Data}.
```

若在门锁着时按下了一个按钮，当前按下的按钮与下一个正确的按钮对比，根据结果的不同，门可能解锁并且**gen_statem**进入**open**状态，或者门依然保持**locked**状态。

若按下的按钮不对，服务器数据将会重新回到初始的密码序列。

在**open**状态，任何按钮都会把门关闭，所有事件都会取消事件计时器，所以在 一个按钮事件过后没有超时事件发生。

### 4.9 Time-Outs状态

当给予一个正确密码时门将会被解锁并且**locked/2**将会返回下面的元组：

```
{next_state,open,Data#{remaining := Code},10000};
```

10,1000是超时的毫秒值，超过这个时间会导致超时，然后**StateName(timeout, 10000, Data)**被调用，当门以**open**状态保持10秒会导致超时发生，然后门又会被锁。

```
open(timeout, _,  Data) ->
    do_lock(),
    {next_state,locked,Data};
```

### 4.10 所有状态事件

事件有时会在**gen_statem**的任何状态下到达。在通用状态处理函数中处理这些是非常方便的，事件的所有状态函数调用不特定于状态。 

**code_length/0**函数返回正确密码的长度，我们调度所有不是状态特定（state-specific）的事件到通用函数**handle_event/3**:

```
...
-export([button/1,code_length/0]).
...

code_length() ->
    gen_statem:call(?NAME, code_length).

...
locked(...) -> ... ;
locked(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

...
open(...) -> ... ;
open(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

handle_event({call,From}, code_length, #{code := Code} = Data) ->
    {keep_state,Data,[{reply,From,length(Code)}]}.
    
This example uses gen_statem:call/2, which waits for a reply from the server. The reply is sent with a {reply,From,Reply} tuple in an action list in the {keep_state,...} tuple that retains the current state.
```

这个例子中使用了[gen_statem:call/2](http://erlang.org/doc/man/gen_statem.html#call-2)，等待来自服务器的应答。此应答在**{keep_state,...}**元组的动作列表中与**{reply,From,Reply}**元组一起发送，并且保持当前状态。 

### 4.11 单一事件处理

若使用**handle_event_function**,所有的事件都会在[Module:handle_event/4](http://erlang.org/doc/man/gen_statem.html#Module:handle_event-4)中处理,并且当我们首先依据事件分支时我们可以使用
```
...
-export([handle_event/4]).

...
callback_mode() ->
    handle_event_function.

handle_event(cast, {button,Digit}, State, #{code := Code} = Data) ->
    case State of
	locked ->
	    case maps:get(remaining, Data) of
		[Digit] -> % Complete
		    do_unlock(),
		    {next_state, open, Data#{remaining := Code},
                     [{state_timeout,10000,lock}};
		[Digit|Rest] -> % Incomplete
		    {keep_state, Data#{remaining := Rest}};
		[_|_] -> % Wrong
		    {keep_state, Data#{remaining := Code}}
	    end;
	open ->
            keep_state_and_data
    end;
handle_event(state_timeout, lock, open, Data) ->
    do_lock(),
    {next_state, locked, Data}.

...

```

### 4.12 停止

#### 在监督树中

若**gen_statem**是监督树的一部分，则不需要停止函数。**gen_statem**会自动被其supervisor终止。supervisor中设置的[关闭策略(shutdown strategy)](supervisor.md)定义了该怎么做。

如果在终止前要执行清理任务，那关闭策略必须为一个超时值并且在**init/1**函数中**gen_statem**必须通过调用[process_flag(trap_exit, true)](http://erlang.org/doc/man/erlang.html#process_flag-2)来捕获退出信号：

```
init(Args) ->
    process_flag(trap_exit, true),
    do_lock(),
    ...
```

当执行关闭操作时，**gen_statem**会调用回调函数**terminate(shutdown, State, Data)**。

在这个例子中，如果门是开着的那么函数**terminate/3**会将其锁上，所以当监督树终止时，我们不会意外地将门打开：

```
terminate(_Reason, State, _Data) ->
    State =/= locked andalso do_lock(),
    ok.
```

#### 独立的gen_statem

若**gen_statem**不是监督树的一部分，那么可以使用[gen_statem:stop](http://erlang.org/doc/man/gen_statem.html#stop-1)将其停止，通过API函数：

```
...
-export([start_link/1,stop/0]).

...
stop() ->
    gen_statem:stop(?NAME).
```

这使得**gen_statem**调用回调函数**terminate/3**就像一个被监督的服务器（supervised server）一样等待进程终止。

### 4.13 事件超时

超时功能从**gen_statem**的前身[gen_fsm](http://erlang.org/doc/man/gen_fsm.html)继承，为事件超时，也就是说如果事件到达则定时器被取消。能获取到一个事件或者超时，但是不会同时获取到两者。

它由状态转换动作来生成，可能的状态转换动作为**{timeout,Time,EventContent}**或**Time**甚至只是**Time**而不是一个动作列表（后者是**gen_fsm**的一种继承形式）。

这种类型的超时对不活动的动作是很有用的，若在30秒内没有按钮按下在，则重启一个密码序列：

```
...

locked(
  timeout, _, 
  #{code := Code, remaining := Remaining} = Data) ->
    {next_state, locked, Data#{remaining := Code}};
locked(
  cast, {button,Digit},
  #{code := Code, remaining := Remaining} = Data) ->
...
        [Digit|Rest] -> % Incomplete
            {next_state, locked, Data#{remaining := Rest}, 30000};
...
```

每当我们接收到一个按钮事件时，我们启动一个超时为30秒的事件，若我们捕获到一个**timeout**类型事件则重置密码序列。

超时事件能被其他任何的事件取消，所以你可能会得到其他类型的事件或者超时事件。因此，不可能也不需要取消或重启一个超时事件。

### 4.14 Erlang定时器

先前关于超时状态的例子，只起效于超时时间内状态机保持相同状态的情况。并且，超时事件只在没有发生无关干扰的情况下工作。

也许你想在一个状态下启动一个定时器，并在另一个状态下响应超时，也许想不通过改变状态来取消掉超时，再也许你想并行的运行多个超时。这些都可以通过Erlang定时器[erlang:start_timer3,4](http://erlang.org/doc/man/erlang.html#start_timer-4)来完成。

这里会介绍怎样使用Erlang定时器来完成先前例子中的超时状态：

```
...
locked(
  cast, {button,Digit},
  #{code := Code, remaining := Remaining} = Data) ->
    case Remaining of
        [Digit] ->
	    do_unlock(),
	    Tref = erlang:start_timer(10000, self(), lock),
            {next_state, open, Data#{remaining := Code, timer => Tref}};
...

open(info, {timeout,Tref,lock}, #{timer := Tref} = Data) ->
    do_lock(),
    {next_state,locked,maps:remove(timer, Data)};
open(cast, {button,_}, Data) ->
    {keep_state,Data};
...
```

当我们改变**locked**状态时，从map中移除**timer**键并不是必须的。因为我们只能使用一个较新的**timer**map值来进入状态**open**。

若由于某些其他事件导致你必须取消定时器，你可以使用[erlang:cancel_timer(Tref)](http://erlang.org/doc/man/erlang.html#cancel_timer-2)。注意,超时消息在调用此函数过后是不会被接收到的，除非你在先前延迟了（下一节将会介绍），所以确保不会意外的延迟此类消息。同时也要注意，超时消息可能会在你取消定时器之前到达，所以你可能必须要从进程信箱中读取此类消息，具体取决于[erlang:cancel_timer(Tref)](http://erlang.org/doc/man/erlang.html#cancel_timer-2)的返回值。

另外一个方法来处理迟到超时的方式是不取消它，如果我们知道它是迟到的那么当它到达时我们可以将其丢弃。

### 4.15 延迟（Postponing ）事件

若你想在当前的状态下丢弃一个特定事件并且在将来的某种状态中来处理，可以延迟此事件。状态更改后会重试延迟事件，也就是：**OldState =/= NewState**。

延迟事件由状态转换动作**postpone**生成。

在这个例子中，代替在**open**状态中舍弃按钮事件的方式，我们可以延迟它们，然后它们将排队并且在**locked**状态中处理。

```
...
open(cast, {button,_}, Data) ->
    {keep_state,Data,[postpone]};
...
```

由于延迟事件只在状态改变时重试，你必须考虑保存状态数据的位置。你可以将其放在服务器**Data**或者它自己的**State**中，例如通过两个具有或多或少相同的状态来保存布尔值，或者用[callback mode handle_event_function](http://erlang.org/doc/man/gen_statem.html#type-callback_mode)来使用复杂的状态。一个值的变化导致了事件集合的改变，如果处理了这种情况，那么值将会被保存在State中。否则，不会有延迟事件会被重试，除了服务器Data改变。

延迟事件并不是很重要，但是如果你以后决定延迟一些事件，会导致它们出现与预期不同的状态缺陷，这样很难找出bug。

#### 模糊状态图

状态图没有指定如何处理图中未示出的特定状态事件的情况并不罕见。希望这在相关的文本或上下文中有所描述。

Possible actions: ignore as in drop the event (maybe log it) or deal with the event in some other state as in postpone it.

可能的操作：舍弃事件（或许记录它）或者在其他某些事件中处理。

#### 选择性接收

Erlang的选择性接收语句经常用来在简单的Erlang代码中描述简单的状态机实例。下面的代码是第一个例子的一种实现方式：

```
-module(code_lock).
-define(NAME, code_lock_1).
-export([start_link/1,button/1]).

start_link(Code) ->
    spawn(
      fun () ->
	      true = register(?NAME, self()),
	      do_lock(),
	      locked(Code, Code)
      end).

button(Digit) ->
    ?NAME ! {button,Digit}.

locked(Code, [Digit|Remaining]) ->
    receive
	{button,Digit} when Remaining =:= [] ->
	    do_unlock(),
	    open(Code);
	{button,Digit} ->
	    locked(Code, Remaining);
	{button,_} ->
	    locked(Code, Code)
    end.

open(Code) ->
    receive
    after 10000 ->
	    do_lock(),
	    locked(Code, Code)
    end.

do_lock() ->
    io:format("Locked~n", []).
do_unlock() ->
    io:format("Open~n", []).
```