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

**start_link**调用了函数**gen_server:start_link/4**,此函数分裂(spawns)并且连接到一个新的gen_server进程

- 第一个参数{local, ch3}用于指定名称。调用完成后gen_server在本地被注册为ch3。
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

**gen_server:start_link**是同步的,当**gen_server**没有初始化和准备好接收数据时,它是不会返回的。
如果**gen_server**是监督树的一部分,那么必须使用**gen_server:start_link**,也就是说一个**gen_server**不是监督树的一部分。