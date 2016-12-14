### 4 gen_statem行为

本章应该与STDLIB手册中的[gen_statem](http://erlang.org/doc/man/gen_statem.html)一节一起阅读,包含了所有的接口函数与回调函数的定义。

#### 4.1 事件驱动的状态机

已建立的自动机理论不太关心状态转换时如何触发的，但是假设输出是输入（和状态）函数并且它们是某种值。
对于一个事件驱动的状态机，输入事件触发一个状态转换并且输出是在状态转换期间执行的动作。与有限状态机的数学模型近似，可以描述为一组关系集合，就像下面这种形式：
```
State(S) x Event(E) -> Actions(A), State(S')
```

他们的关系可以解释成：若我们正处于**S**状态并且触发了事件**E**，我们要执行**A**操作并且转换状态到**S'**。注意**S'**可能与**S**相等。

当**A**和**S'**只依赖于**S**和**E**时，这种类型的状态机我们叫做[Mealy Machine](https://en.wikipedia.org/wiki/Mealy_machine)。

就像大部分的**gen_**行为，**gen_statem**保存除服务器状态之外的数据。正因为这样，对状态（假设有足够的虚拟内存）或不同输入的数量没有限制，由此行为实现的状态机实际上是[图灵完备](http://stackoverflow.com/questions/7284/what-is-turing-complete)。但是感觉更像是事件驱动的Mealy机。

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

当你忧郁哥常规状态图时，这个模式很适合。像本章中提到的那些一样，描述属于状态的所有事件和动作，并且每个状态都有唯一的名称。

使用**handle_event_function**时，可以随意使用混合策略，所有的时间和状态都在一个相同的回调函数里处理。

当你想关注当时的一种事件或者状态时，这种模式同样能很好的工作，但[Module:handle_event/4](http://erlang.org/doc/man/gen_statem.html#Module:handle_event-4)会快速增长而无法分支到辅助函数。 

这种模式允许使用非原子的状态，例如复杂或者分层状态。对于协议两端的客户端和服务端的状态图有很大程度上的相似，你可以使用 **{StateName,server}**或者**{StateName,client}**状态中的**StateName**来决定在该代码里哪处处理该状态的大部分事件。元组中的第二个参数用来选择是否处理特殊的客户端或服务端事件。 

### 4.3 例子

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

### 4.4  启动gen_statem

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

### 4.5 处理事件

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

### 4.6 Time-Outs事件

当一个给予了一个正确的密码时门将会被解锁并且**locked/2**将会返回下面的元组：

```
{next_state,open,Data#{remaining := Code},10000};
```

10,1000是超时的毫秒值，超过这个时间会导致超时，然后**StateName(timeout, 10000, Data)**被调用，当门以**open**状态保持10秒会导致超时发生，然后门又会被锁。

```
open(timeout, _,  Data) ->
    do_lock(),
    {next_state,locked,Data};
```

### 4.7 所有状态事件

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

### 4.8 单一事件处理

若使用**handle_event_function**,所有的事件都会在[Module:handle_event/4](http://erlang.org/doc/man/gen_statem.html#Module:handle_event-4)中处理,并且当我们首先依据事件分支时我们可以使用