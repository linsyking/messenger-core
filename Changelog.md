# Changelog

## [15.0.0] *Breaking Change* - 7 January 2025

### Features

- Switched to elm-regl rendering backend with WebGL support
- Add many API docs

## [14.0.0]

### Features

- Add more features to audio

## [13.0.0] *Breaking Change* - 21 June 2024

### Features

- Using `InternalData` to render instead of `GlobalData`
- Remove `SOMMsg` related to transition and scene context management
- Add basic support for global component
- `AllScenes` changed to `Dict` instead of `List`
- Use new `elm-canvas` `DrawText` API to draw multi-line text

### Fix

- Viewport initialization in Elm

## [12.1.0]

### Features

- Add `TextBox` rendering helper functions
- Add test modules inside core repository

## [12.0.4]

### Fix

- Transition time not correct because using delta

## [12.0.3]

### Fix

- Using wrong `env` in `updateRemain`

## [12.0.2]

### Fix

- `updateRemain` order error

## [12.0.0] - 22 May 2024

### Features

- Better audio support
- Tick events with delta

### Fix

- `SOMHandler` fix

## [11.1.0]

### Features

- Add `updateComponentsWithBlock`

## [11.0.0]

### Fix

- Component storage type
- Type sugar

## [10.0.0]

### Fix

- Types of updating components and messages

## [9.2.0]

### Features

- Implement loadSpriteNum to count loading resources
- Block SOMMsg when transition

## [9.1.0]

### Features

- Scene Prototype helper types and function

## [9.0.0]

### Features

- Add Scene Context Management

## [8.2.1]

### Chore

- `if x then True else False` to `x`

## [8.2.0]

### Features

- Add type sugar for raw scene
- Add matcher for component

## [8.0.0] *Breaking Change* - 12 May 2024

### Features

- Move Messenger libraries to core

## [7.0.0 Stable] - 26 Jul 2023

### Fix

- Add patcher

## [6.0.0]

### Features

- Add `updaterec`, which splits update functions into two

## [5.0.0]

### Features

-  Add `Cleaner` type and related support

## [4.1.0]

### Features

- Expose Target Updater

## [4.0.1]

### Fix

- Change signature symbols

## [4.0.0]

### Fix

- Remove `init` from `GeneralModel`

## [3.0.2]

### Fix

- Use `foldl` to fix the order of updating

## [3.0.1]

### Features

- Use `fodlr` instead of `foldl` to update

## [3.0.0], [2.0.0]

### Fix

- General Model signatures

## [1.1.0]

### Features

- Add `GeneralModel`
