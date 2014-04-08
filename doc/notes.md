## live code reloading

This is possible with sane lifecycle management.

External global state: state is separate from behavior.

Reload trigger thet reloads code in the component.

## Things to work on 

Basic code reloading for one system being worked on.  
See if we can use this as a base for devcards system.

look at IReactiveDerive protocol for frontier - takes previous and current state
(where when the state changes in a certain way effects get enqueued).
This could be a bad idea.

other protocols IValidate - validates state on each state change

deliver state and previous state to render functions

look at maintaining the rendered-to nodes in the document like react.

look at integration with om

rewrite server, should only send code shanges after the code has actually changed.

** throttle rendering to prevent gratuitous rerenders.


