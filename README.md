To start project:

```
yarn install && yarn start
```

Tap on open, the "modal" opens. Tap on the modal to close, and you get this
error:

```
Error: Failed pattern match at Data.Maybe (line 268, column 1 - line 268, column 46): Nothing2 bundled.js:2788:15
    fromJust http://127.0.0.1:4201/bundled.js:2788
    unsafeSetCell http://127.0.0.1:4201/bundled.js:39115
    interpretHalogenHook http://127.0.0.1:4201/bundled.js:39663
    interpretHalogenHook http://127.0.0.1:4201/bundled.js:39673
```
