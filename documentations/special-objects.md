# Special Objects for Program State

GreenThumb provides a few special objects to be used as parts of a program state. Each of the special object has two implementations; one in Racket (xxx-racket.rkt) and another in Rosette (xxx-rosette.rkt). From a developer's point of view, there is no different between the two implementations. Their APIs are pretty much the same. Deverlopers may need to manipulate these objects in a few functions, including `interpret` function (in `simulator-racket.rkt` and `simulator-rosette.rkt`) and `correctness-cost` function (in `stochastic.rkt`). In the scenario that the developers override `interpret-inst` function (inverse interpreter) in `inverse.rkt`, they may have to manipulate these objects as well. We will describe APIs to manipulate these objects.

## 1. Memory Object
##### (`memory-rosette%` and `memory-racket%`)
This object is use to represent memory space. It can store unbounded amount of data. The memory object store content as collections of pairs; each pair contains an address and a memory value. In fact, it has two collections of pairs. The first collection called `init` contains initial memory content that is read. The second collection called `update` contains memory content that is modified.

##### 1.1 Useful Methods for `interpret` (in `simulator-racket/rosette.rkt`)
- **(clone [ref #f])** clones returns a clone of this memory object. Before modifying a memory object in `interpret` function, developers need to clone it first. Modifying the memory object without cloning it will result in mutating the input program state. If the `interpret` function is invoked with the optional argument `ref` (an expected output state), developers must extract memory from `ref` and pass that as an argument to this `clone` method. A memory object that is created with a `ref` memory object is allowed to be read and modified only at the locations the `ref` memory object is read and modified. This significantly reduces the size of the search space the superoptimizer searches over.
- **(load addr)** returns a value of memory at address `addr`.
- **(store addr val)** stores a value `val` at address `addr`.

##### 1.2 Useful Methods for `interpret-inst` (in `inverse.rkt`)
- **(del addr)** deletes a value at address `addr`. It removes a pair with address `addr` from the `update` memory content.
- **(get-update-addr-val)** returns a list of (addr,val) pairs in the `update` memory content.
- **(get-update-addr-with-val val)** returns a list of addresses whose value is equal to `val`  from the `update` memory content.
- **(get-update-addr-with-val val)** returns a list of addresses whose value is equal to `val` from both the `init` and `update` memory content.
- **(get-available-addr)** returns a list of addresses from both the `init` and `update` memory content.


##### 1.3 Useful Methods for `correctness-cost` (in `stochastic.rkt`)
- **(correctness other diff-func bit)** returns a correctness cost of `other` memory object against `this` memory object. If they contains values (`v1` and `v2`) at an address `a`, the two values will contribute to the total cost by `(diff-func v1 v2)`. For each address that appears in `this` but not in `other`, the total cost is increased by `bit`.
- 
##### 1.4 Equivalence
In GreenThumb, two memory objects are equivalent, if their modified contents are the same; if their `update`s contain the same sets of pairs.

## Input Queue Object
##### (`queue-in-rosette%` and `queue-in-racket%`)
This object represents values received from external environments such as values sent from other cores, and values received from sensors.

##### 1.1 Useful Methods for `interpret` (in `simulator-racket/rosette.rkt`)
- **(clone [ref #f])**
- **(pop)** returns the next value received.

##### 1.2 Useful Methods for `interpret-inst` (in `inverse.rkt`)
- **(pop-inverse val)** reverses the pop action and puts `val` back to the received queue.

##### 1.3 Useful Methods for `correctness-cost` (in `stochastic.rkt`)
- **(correctness other diff-func bit)**

##### 1.4 Equivalence
In GreenThumb, two input queue objects are equivalent, if the same number of values are popped from the two objects.

## Output Queue Object
##### (`queue-out-rosette%` and `queue-out-racket%`)
This object represents values sent to external environments such as values sent to other cores or sensors.
##### 1.1 Useful Methods for `interpret` (in `simulator-racket/rosette.rkt`)
- **(clone [ref #f])**
- **(push val)** pushes `val` to the sent queue.

##### 1.2 Useful Methods for `interpret-inst` (in `inverse.rkt`)
- **(push-inverse)** reverses the push action and returns the value pushed to the sent queue.

##### 1.3 Useful Methods for `correctness-cost` (in `stochastic.rkt`)
- **(correctness other diff-func bit)**

##### 1.4 Equivalence
In GreenThumb, two output queue objects are equivalent, if the values push to the two output queues are the same in the same oreder.
