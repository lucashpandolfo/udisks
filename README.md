UDisks: A toy cl udisks bindings
================================

Mostly centered in block_devices


UDisks Manager for Stumpwm
==========================

Setup
-----

Copy or symlink the stumpwm/udisks-manager.lisp over to your stumpwm
plugins directory and add the following code to your .stumpwmrc file

``` Lisp
;;Load the module and dependences
(ql:quickload "udisks")
(load-module "udisks-manager")

;;Handy commands
(stumpwm:defcommand show-devices () ()
  "Show devices"
  (stumpwm-udisks:show-devices-menu))

(stumpwm:defcommand launch-udisks-manager () ()
  "Launch (or relaunch) udisk manager"
  (stumpwm-udisks:udisks-manager-launch))

;; Launch at init
(launch-udisks-manager)
```


Screenshots
-----------

![New device](screenshots/new.png)
![Devices menu](screenshots/menu.png)
![Actions menu](screenshots/actions.png)
![Actions menu2](screenshots/actions2.png)
![Unmount device](screenshots/unmounted.png)
