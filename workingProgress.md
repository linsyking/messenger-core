```bash
Messenger
    ├── Audio
    │   ├── Audio.elm  #done
    │   ├── Base.elm   #done
    │   └── Internal.elm #excluded
    ├── Base.elm #done
    ├── Component
    │   ├── Component.elm #done
    │   └── GlobalComponent.elm #delayed
    ├── Coordinate
    │   ├── Coordinates.elm #done
    │   └── HTML.elm #delayed
    ├── GeneralModel.elm #done
    ├── Layer
    │   └── Layer.elm #done
    ├── Model.elm #excluded
    ├── Recursion.elm #excluded
    ├── Render
    │   ├── Shape.elm #done
    │   ├── Sprite.elm #done
    │   ├── TextBox.elm #done
    │   └── Text.elm #done
    ├── Resources
    │   └── Base.elm #excluded
    ├── Scene
    │   ├── LayeredScene.elm #done
    │   ├── Loader.elm #delayed
    │   ├── RawScene.elm #done
    │   └── Scene.elm #partial
    ├── UI #excluded
    │   ├── Init.elm
    │   ├── Input.elm
    │   ├── SOMHandler.elm
    │   ├── Subscription.elm
    │   ├── Update.elm
    │   └── View.elm
    ├── UI.elm #excluded
    └── UserConfig.elm #excluded
```

Notes:

-   UI.elm was excluded since it only contains messenger-generated functions.
-   /UI was excluded since it will not be exposed to user.
-   Scene/loader was excluded since it only contains messenger-generated functions.
-   /Resource/Base was excluded since it is only used internally.
-   All portable-related and global-component-related functions was excluded since I am not familiar with them. They are commonly seen in Scene/Scene and Component/GlobalComponent.
-   MMSg need more docs.
-   Recursionand Model excluded since they are used only internally.
-   Generally All files under Scene has relatively poor doc since they are too basic so that I am not very confident about them. Please take care of them.
-   Note that I added some grammar sugar(MiddleSteps) in Layer.Layer and Component.Component. Please test all of them except UpdateMiddleStep(it has proved to be useful in our P2).
-   UserConfig.elm excluded since it has been automatically handled in resource.elm. I am considering to add an example at the doc introduction
-   Enjoy your day!
