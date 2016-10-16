# Special Objects for Program State

GreenThumb provides the following special objects to be used as parts of a program state:
- [Memory Object](#memory)
- [Input Queue Object](#input-queue)
- [Onput Queue Object](#output-queue)


Each of the special object has two implementations; one in Racket (xxx-racket.rkt) and another in Rosette (xxx-rosette.rkt). From a developer's point of view, there is not much difference between the two implementations; their APIs are pretty much the same. Deverlopers may need to manipulate these objects in a few functions, including `interpret` function (in `simulator-racket.rkt` and `simulator-rosette.rkt`) and `correctness-cost` function (in `stochastic.rkt`). In the scenario that the developers override `interpret-inst` function (inverse interpreter) in `inverse.rkt`, they may have to manipulate these objects as well. We now describe APIs to manipulate these objects. Note that these provided methods should be invoked by:
```racket
(send* <object> <method> <arg> ...)
```
`send*` should be used instead of Racket primitive [send](https://docs.racket-lang.org/reference/ivaraccess.html#%28part._methodcalls%29). `send*` is a macro defined in `ops-rosette.rkt` and `ops-racket.rkt`.

<a name="memory"></a>
## 1. Memory Object
##### (`memory-rosette%` and `memory-racket%`)
This object is use to represent memory space. It can store unbounded amount of data. A memory object stores content as pairs; each pair contains an address and a memory value. An address and a value in a memory object have the same bitwidth as defined in `machine%`.
A memory object maintains two collections of pairs. The first collection called `init` contains initial memory content that is read. The second collection called `update` contains memory content that is modified. The program state element type of a memory object can be obtained by calling `(get-memory-type)`, which is defined in `special.rkt`.

##### 1.1 Useful Methods for `interpret` (in `simulator-racket/rosette.rkt`)
- **(clone [ref #f])** returns a clone of this memory object. Before modifying a memory object in `interpret` function, developers need to clone it first. Modifying the memory object without cloning it will result in mutating the input program state. If the `interpret` function is invoked with the optional argument `ref` (an expected output state), developers must extract memory from `ref` and pass that as an argument to this `clone` method. A memory object that is created with a `ref` memory object is allowed to be read and modified only at the locations the `ref` memory object is read and modified. This significantly reduces the size of the search space the superoptimizer searches over.
- **(load addr)** returns the value of memory at address `addr`.
- **(store addr val)** stores a value `val` at address `addr`.

##### 1.2 Useful Methods for `interpret-inst` (in `inverse.rkt`)
- **(del addr)** deletes the value at address `addr`. It removes a pair with address `addr` from the `update` memory content.
- **(get-update-addr-val)** returns a list of (addr,val) pairs in the `update` memory content.
- **(get-update-addr-with-val val)** returns a list of addresses whose value is equal to `val`  from the `update` memory content.
- **(get-update-addr-with-val val)** returns a list of addresses whose value is equal to `val` from both the `init` and `update` memory content.
- **(get-available-addr)** returns a list of addresses from both the `init` and `update` memory content.


##### 1.3 Useful Methods for `correctness-cost` (in `stochastic.rkt`)
- **(correctness other diff-func bit)** returns a correctness cost of `other` memory object compared against `this` memory object. If they contains values (`v1` and `v2`) at an address `a`, the two values will contribute to the total cost by `(diff-func v1 v2)`. For each address that appears in `this` but not in `other`, the total cost is increased by `bit`.

##### 1.4 Equivalence
In GreenThumb, two memory objects are equivalent, if their modified contents are the same; this is, if their `update`s contain the same sets of pairs.

<a name="input-queue"></a>
## 2. Input Queue Object
##### (`queue-in-rosette%` and `queue-in-racket%`)
This object represents values received from external environments such as values received from other cores or sensors. Currently, this object uses a vector of size 4 to store received values. Increase the size in `queue-in-rosette%` and `queue-in-racket%` if necessary. The program state element type of an input queue object can be obtained by calling `(get-queue-in-type)`, which is defined in `special.rkt`.

##### 2.1 Useful Methods for `interpret` (in `simulator-racket/rosette.rkt`)
- **(clone [ref #f])** returns a clone of this input queue object. Before modifying an input queue object in `interpret` function, developers need to clone it first. Modifying the input queue object without cloning it will result in mutating the input program state. If the `interpret` function is invoked with the optional argument `ref` (an expected output state), developers must extract an input queue from `ref` and pass that as an argument to this `clone` method. 
- **(pop)** returns the next value received.

##### 2.2 Useful Methods for `interpret-inst` (in `inverse.rkt`)
- **(pop-inverse val)** reverses the pop action and puts `val` back to the received queue.

##### 2.3 Useful Methods for `correctness-cost` (in `stochastic.rkt`)
- **(correctness other diff-func bit)** returns a correctness cost of `other` input queue object compared against `this` input queue object. The cost is `|n1 - n2| * bitwidth`, where `n1` is the number of values popped from `this`, and `n2` is the number of values popped from `other`.

##### 2.4 Equivalence
In GreenThumb, two input queue objects are equivalent, if the same number of values are popped from the two objects.

<a name="output-queue"></a>
## 3. Output Queue Object
##### (`queue-out-rosette%` and `queue-out-racket%`)
This object represents values sent to external environments such as values sent to other cores or sensors. Currently, this object uses a vector of size 4 to store sent values. Increase the size in `queue-out-rosette%` and `queue-out-racket%` if necessary. The program state element type of an input queue object can be obtained by calling `(get-queue-out-type)`, which is defined in `special.rkt`.

##### 3.1 Useful Methods for `interpret` (in `simulator-racket/rosette.rkt`)
- **(clone [ref #f])**  returns a clone of this output queue object. Before modifying an output queue object in `interpret` function, developers need to clone it first. Modifying the output queue object without cloning it will result in mutating the input program state. If the `interpret` function is invoked with the optional argument `ref` (an expected output state), developers must extract an output queue from `ref` and pass that as an argument to this `clone` method. An output queue object that is created with a ref memory object is allowed to store only values that are stored in `ref` in the same order. This significantly reduces the size of the search space the superoptimizer searches over.
- **(push val)** pushes `val` to the sent queue.

##### 3.2 Useful Methods for `interpret-inst` (in `inverse.rkt`)
- **(push-inverse)** reverses the push action and returns the value pushed to the sent queue.

##### 3.3 Useful Methods for `correctness-cost` (in `stochastic.rkt`)
- **(correctness other diff-func bit)** returns a correctness cost of `other` output queue object compared against `this` output queue object. The cost is `|n1 - n2| * bitwidth`, where `n1` is the number of values pushed to `this`, and `n2` is the number of values pushed to `other`.

##### 3.4 Equivalence
In GreenThumb, two output queue objects are equivalent, if the values pushed to the two output queues are the same in the same order.
