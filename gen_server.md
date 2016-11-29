## 2 gen_server Behaviour
##### 本章应该与stdlib手册中的[gen_server](http://erlang.org/doc/man/gen_server.html)一节一起阅读,对接口函数和回调函数都有描述

###2.1 客户端-服务端原则

客户端-服务端模式表示为一个中心服务器与任意多数目的客户端。客户端-服务端模式被用来管理资源,多个不同的客户端分享相同的资源,服务器负责管理此资源

![client-server model](http://erlang.org/doc/design_principles/clientserver.gif)

### 2.2 例子

在[overview](<overview.md>)中有一个用纯Erlang编写的简单服务器,可以用**gen_server**重新实现,在回调模块中是这样:

```
-module(ch3).
-behaviour(gen_server).

-export([start_link/0]).
-export([alloc/0, free/1]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, ch3}, ch3, [], []).

alloc() ->
    gen_server:call(ch3, alloc).

free(Ch) ->
    gen_server:cast(ch3, {free, Ch}).

init(_Args) ->
    {ok, channels()}.

handle_call(alloc, _From, Chs) ->
    {Ch, Chs2} = alloc(Chs),
    {reply, Ch, Chs2}.

handle_cast({free, Ch}, Chs) ->
    Chs2 = free(Ch, Chs),
    {noreply, Chs2}.
```
此代码的解释在下一章。

### 2.3 启动Gen_Server

在上节的例子中,**gen_server**通过调用**ch3:start_link()**启动:
```
start_link() ->
    gen_server:start_link({local, ch3}, ch3, [], []) => {ok, Pid}
```

**start_link**调用了函数**gen_server:start_link/4**,此函数分裂(spawns)并且链接到一个新的gen_server进程

- 第一个参数{local, ch3}用于指定名称。调用完成后gen_server在局部被注册为ch3。
若名称被省略,那么gen_server不会被注册,而必须使用其pid。name参数同样也可以为{global, Name},在这种情况下,gen_server使用global:register_name/2注册。
- 第二个参数ch3为回调模块的名字,也就是回调函数所在的模块。 
接口函数(start_link,alloc,free)位于与回调函数(init,handle_call)相同的模块中。这通常是一种好的编程实践,具有对应于包含在一个模块中的一个进程的代码。
- 第三个参数[]按照原样传递给回调函数init,这里的init函数不需要任何的数据并且忽略了这个参数。
- 第四个参数[]是一个设置列表,具体详见**gen_server(3)**手册页。

若名称注册成功,新的**gen_server**进程会调用回调函数**ch3:init([])**。**init**应该返回{ok，State}，其中State是**gen_server**的内部状态。在这个例子中,state是可用的channels。
```
init(_Args) ->
    {ok, channels()}.
```

**gen_server:start_link**是同步的,当**gen_server**没有初始化和准备好接收请求时,它是不会返回的。
如果**gen_server**是监督树的一部分,即由一个supervisor启动,那么必须使用**gen_server:start_link**。另外有一个**gen_server:start**函数,可以启动一个独立的服务器,也就是说一个**gen_server**不是监督树的一部分。

### 2.4 同步请求 - Call

同步请求**alloc()**由**gen_server:call/2**实现:
```
alloc() ->
    gen_server:call(ch3, alloc).
```

ch3是gen_server的名称并且必须与用于启动它的名称一致,alloc是实际的请求。

该请求被转换成一条消息并且发送到**gen_server**。当请求到达时,**gen_server**调用**handle_call(Request, Form State)**,返回一个元祖**{reply, Reply, State1}**。**Reply**是发送到客户端的应答并且**State1**是**gen_server**的一个新的状态值.

```
handle_call(alloc, _From, Chs) ->
    {Ch, Chs2} = alloc(Chs),
    {reply, Ch, Chs2}.
```

在这个例子中,**Ch**是分配好的频道,新的状态值**Chs2**是剩余可用频道的集合。

因此,**ch3:alloc()**返回已经分配好的频道**Ch**,**gen_server继续等待**新的请求并且有可用频道的更新列表。

###异步请求 - Cast

异步请求**free(Ch)**由**gen_Server:cast/2**实现:
```
free(Ch) ->
    gen_server:cast(ch3, {free, Ch}).
```

ch3是**gen_server**的名称,{free, Ch}是实际的请求。 

该请求被转换成一条消息并且发送到**gen_server**。**cast**, and thus **free**, then returns **ok**.

当请求到达时,**gen_server**调用**handle_cast(Request, State)**,返回一个元祖**{noreply, State1}**,**State1**是**gen_server**的一个新的状态值.
```
handle_cast({free, Ch}, Chs) ->
    Chs2 = free(Ch, Chs),
    {noreply, Chs2}.
```

在这个例子中,新状态是可用频道**Chs2**的更新列表,**gen_server**已准备好接收新的请求。

### 2.6 停止

#### 在监督树中

如果**gen_server**是监督树的一部分,那么并不需要停止函数。**gen_server**会被它的supervisor自动终止。supervisor中设置的[关闭策略(shutdown strategy)](supervisor.md)定义了该怎么做。

如果在终止前需要执行清理,关闭策略必须为一个超时值,**gen_server**必须设置成在**init**函数中捕获退出信号。当执行关闭时,**gen_server**会调用回调函数**terminate(shutdown, State)**:

```
init(Args) ->
    ...,
    process_flag(trap_exit, true),
    ...,
    {ok, State}.

...

terminate(shutdown, State) ->
    ..code for cleaning up here..
    ok.
```

#### 独立的Gen_Servers

如果**gen_server**不是监督树的一部分,那么停止函数将会很有用,例如:

```
...
export([stop/0]).
...

stop() ->
    gen_server:cast(ch3, stop).
...

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({free, Ch}, State) ->
    ....

...

terminate(normal, State) ->
    ok.
```

处理**stop**请求的回调函数返回一个元祖**{stop,normal,State1}**,其中**normal**表示它是正常终止的,**State1**是**gen_server**的新状态。这使得**gen_server**调用**terminate(normal, State1)**过后它会正常终止。

### 2.7 处理其他消息

如果**gen_server**能够接收除请求之外的其他消息,回调函数**handle_info(Info, State)**必须实现来处理它们。例如退出消息,如果**gen_server**被链接到其他进程并且捕获退出信号。

```
handle_info({'EXIT', Pid, Reason}, State) ->
    ..code to handle exits here..
    {noreply, State1}.
```

**code_change**方法也必须实现。

```
code_change(OldVsn, State, Extra) ->
    ..code to convert state (and more) during code change
    {ok, NewState}.
```
