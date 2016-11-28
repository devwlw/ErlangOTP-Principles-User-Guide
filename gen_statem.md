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