# Erlang/OTP设计原则中文文档
[官方地址](http://erlang.org/doc/design_principles/des_princ.html)

##目录
* [Overview](#1)
* [gen_server Behaviour](#2)
* [gen_fsm Behaviour](#3)


<h2 id="1">1 Overview</h2>
#####OTP设计原则定义了如何在进程，模块，目录方面构建Erlang代码

###1.1 监督树
监督树(**supervision tree**)在Erlang/OTP中是一个基本的概念。这是一种基于**workers**与**supervisors**的进程结构模型:

- workers是一些执行计算的进程，处理实际的计算工作
- supervisors是用于监控workers的进程，当worker出现错误supervisor可以重启它
- 监督树是supervisors与workers的代码分层结构，这使得可以设计和编写容错软件

下面的图表中，方框代表supervisors圆圈代表workers:
![figure1.1 Supervision Tree](http://erlang.org/doc/design_principles/sup6.gif)
###1.2 行为
　　在一个监督树中，许多进程有相似的结构，他们遵循着类似的模式。比如supervisors有着相同的结构，他们之间唯一的区别是他们监督的进程。大部分workers是一种server-client关系，有限状态机或者事件处理器比如错误日志
　　行为是一系列相同模式的形式化，这样做是为了在通用部分（行为模块）与特定部分（回调模块）中划分进程的代码。行为模块是Erlang/OTP中的一部分，为了实现一个进程，比如说一个supervisor，只需要实现回调模块，即导出一个预定义的函数集合（**callback functions**）。
　　以下实例说明如何将代码划分为通用和特定部分，下面是一个简单的服务器代码（纯Erlang编写），用于跟踪多个“频道”。其他的进程可以使用**alloc/0**与**free/1**来分配或释放频道。
```
-module(ch1).
-export([start/0]).
-export([alloc/0, free/1]).
-export([init/0]).

start() ->
    spawn(ch1, init, []).

alloc() ->
    ch1 ! {self(), alloc},
    receive
        {ch1, Res} ->
            Res
    end.

free(Ch) ->
    ch1 ! {free, Ch},
    ok.

init() ->
    register(ch1, self()),
    Chs = channels(),
    loop(Chs).

loop(Chs) ->
    receive
        {From, alloc} ->
            {Ch, Chs2} = alloc(Chs),
            From ! {ch1, Ch},
            loop(Chs2);
        {free, Ch} ->
            Chs2 = free(Ch, Chs),
            loop(Chs2)
    end.
```
#####server部分代码可以重写为一个通用部分*server.erl*:
```
-module(server).
-export([start/1]).
-export([call/2, cast/2]).
-export([init/1]).

start(Mod) ->
    spawn(server, init, [Mod]).

call(Name, Req) ->
    Name ! {call, self(), Req},
    receive
        {Name, Res} ->
            Res
    end.

cast(Name, Req) ->
    Name ! {cast, Req},
    ok.

init(Mod) ->
    register(Mod, self()),
    State = Mod:init(),
    loop(Mod, State).

loop(Mod, State) ->
    receive
        {call, From, Req} ->
            {Res, State2} = Mod:handle_call(Req, State),
            From ! {Mod, Res},
            loop(Mod, State2);
        {cast, Req} ->
            State2 = Mod:handle_cast(Req, State),
            loop(Mod, State2)
    end. 
```
#####回调模块*ch2.erl*:
```
-module(ch2).
-export([start/0]).
-export([alloc/0, free/1]).
-export([init/0, handle_call/2, handle_cast/2]).

start() ->
    server:start(ch2).

alloc() ->
    server:call(ch2, alloc).

free(Ch) ->
    server:cast(ch2, {free, Ch}).

init() ->
    channels().

handle_call(alloc, Chs) ->
    alloc(Chs). % => {Ch,Chs2}

handle_cast({free, Ch}, Chs) ->
    free(Ch, Chs). % => Chs2
```

####请注意：
* **server**中的代码可以用来重复构建多种不同的服务
* 服务名，在这个例子中为原子**ch2**，对客户端的函数是不可见的，这意味着可以在不影响到客户端的情况下更改名称
* 协议（从服务端接受和发送的消息）同样是不可见的，这是一种良好的编程实践，允许在不使用接口函数改变代码的情况下改变协议
* 扩展**server**的功能时，可以不更改**ch2**或者其他回调模块

　　在**ch1.erl**与**ch2.erl**中，**channels/0**，**alloc/1**，**free/2**的实现被故意略去了，因为他们与示例无关。为了完整性，如下是他们其中的一种实现。这只是一个例子，在实际实现中必须能够处理多种情况，比如通道用完的情况等等。

```
channels() ->
   {_Allocated = [], _Free = lists:seq(1,100)}.

alloc({Allocated, [H|T] = _Free}) ->
   {H, {[H|Allocated], T}}.

free(Ch, {Alloc, Free} = Channels) ->
   case lists:member(Ch, Alloc) of
      true ->
         {lists:delete(Ch, Alloc), [Ch|Free]};
      false ->
         Channels
   end.        
```

　　不使用行为写出来的代码更有效率，但是却牺牲了通用性。能一致性的管理系统中的应用是很重要的。使用行为同样也能让其他的开发者更容易阅读与理解代码，简易的编程结构虽然更高效但是却难于理解。
　　
　　标准的Erlang/OTP行为有:
　　1. **gen_server**
　　　　实现client-server关系的服务器
　　2. **gen_fsm**
　　　　实现有限状态机（Old）
　　3. **gen_statem**
　　　　实现状态机（New）
　　4. **gen_event**
　　　　实现事件处理功能
　　5. **supervisor**
　　　　在监督树中实现一个**supervisor**

　　编译器能识别模块属性<b>-behaviour(Behaviour)</b>，当缺少回调函数时会发出警告，比如：

```
-module(chs3).
-behaviour(gen_server).

3> c(chs3).
./chs3.erl:10: Warning: undefined call-back function handle_call/3
{ok,chs3}

```
###1.3 应用
　　Erlang/OTP带有许多组件，每一种都实现了特定的功能，这些组件在Erlang/OTP术语中叫做应用（**applications**）。Erlang/OTP中的应用有Mnesia，它拥有数据库编程中的所有功能，还有Dubgger。基于Erlang/OTP的最小系统由下面两个应用组成：

* Kernel - 运行Erlang的必要功能
* STDLIB - Erlang标准库

　　应用的概念同样适用于程序结构（进程）和目录结构（模块）。最简单的应用没有任何进程，但是包括一个功能性模块集合，这样的应用叫做库应用程序，比如说STDLIB。使用标准行为很容易就能将带有进程的应用实现为一个监督树。
　　在[应用](#8)一章描述了怎样编写应用。

###1.4 发行版本
　　发行版本是从Erlang/OTP应用程序的子集和一组用户特定应用程序构成的完整系统。
　　在[发行版本](#11)一章描述了怎样编写。
　　如何在目标系统中安装一个发行版本在系统原则[第二章](http://erlang.org/doc/system_principles/users_guide.html)中有描述

###1.5发行版本处理
　　发行版本处理用于在发行版本的不同版本中进行升级和降级，在正在运行（可能）的系统中，在[发行版本处理](#12)描述。