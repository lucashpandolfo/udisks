;; Disk Manager module for STUMPWM.
;; udisks (and dbus) lisp bindings needs to be loaded before this module

;; ;;Stumpwp interaction
;; (stumpwm:defcommand show-devices () ()
;;   (show-devices-menu))

;; (stumpwm:defcommand launch-udisks-manager () ()
;;   (udisks-manager-launch))

(defpackage #:stumpwm-udisks
  (:use #:cl)
  (:export 
   #:show-devices-menu
   #:udisks-manager-launch
   #:*dir-open-command*))

(in-package #:stumpwm-udisks)

(defparameter *new-device-actions* nil)

(defparameter *dir-open-command* "xterm -e mc ~s")

(defparameter *unmounted-actions-menu* `(("^B^[^2*Mount^]" mount-device)
                                         ("^B^[^6*Mount and open^]" mount-and-open)))

(defparameter *mounted-actions-menu* `(("^B^[^1*Unmount^]" unmount-device)
                                       ("^B^[^4*Open^]" open-device)))

(defun execute-new-device-actions (bus device)
  (loop :for i :in *new-device-actions*
     :do 
     (handler-case (funcall i bus device)
       (error () nil))))

(defun notify-new-device (bus device-path)
  "Show a message with the name of the new device detected. 

TODO: execute custom actions at detection time."

  (let ((name (udisks:block-id-label bus device-path)))
    (stumpwm:message "New device detected: ^[^5*~a^]" name)
    (execute-new-device-actions bus device-path)))


(defun mountablep (bus device-path)
  "Check if a block device is mountable"
  (eq (udisks:block-id-usage bus device-path) :filesystem))

(defun notify-disconnected-device (device-path)
  (declare (ignore device-path))
  (stumpwm:message "Device disconnected"))

(defun get-devices-info ()
  (dbus:with-open-bus (bus (dbus:system-server-addresses))
    (let ((devices (udisks:enumerate-block-devices bus)))
      (let ((mountable (remove-if-not (lambda (dev) (mountablep bus dev)) devices)))
        (loop :for dev :in mountable
           :collect
           (let ((path     (car (udisks:get-mount-points bus dev)))
                 (label    (udisks:block-id-label bus dev))
                 (real-dev (udisks:block-device bus dev)))
             (if (null path)
                 (list :name (if (string-equal label "")
                                 real-dev
                                 label) :mounted nil :object dev)
                 
                 (list :name (if (string-equal label "")
                                 path
                                 label) :mounted t   :object dev))))))))


(defun show-actions-menu (device-info)
  "Displays an action menu for a device."
  (let ((prompt (getf device-info :name)))
    (let ((menu (if (getf device-info :mounted)
                    *mounted-actions-menu*
                    *unmounted-actions-menu*)))

      (let ((action (stumpwm::select-from-menu 
                      (stumpwm:current-screen) 
                      menu prompt)))
        (when action
          (handler-case  (funcall (cadr action)
                                  device-info)
            (error (object) (format nil "~a" object))))))))
          

(defun show-devices-menu ()
  "Shows a menu with the available devices. Mounted ones in
green. Unmounted ones in red. When selected, displays an action menu for the device."
  (let ((devices-info (sort (get-devices-info) 
                            #'string-lessp 
                            :key (lambda (item) (getf item :name)))))

    (flet ((get-colorized-name (device-info)
             (concatenate 'string
                          (if (getf device-info :mounted)
                              "^B^[^2*"
                              "^B^[^1*")
                          (getf device-info :name)
                          "^]")))
      (let ((menu (loop :for device :in devices-info
                     :collect (list (get-colorized-name device) device))))

        (let ((device (stumpwm::select-from-menu (stumpwm:current-screen) menu "Select a device")))
          (when device
            (show-actions-menu
             (cadr device))))))))


(defun main-loop ()
  "Wait for hardware changes and do something with them"
  (let (message)
    (dbus:with-open-bus (bus (dbus:system-server-addresses))
      (dbus:add-match bus :path "/org/freedesktop/UDisks2" :interface "org.freedesktop.DBus.ObjectManager" :member "InterfacesRemoved")
      (dbus:add-match bus :path "/org/freedesktop/UDisks2" :interface "org.freedesktop.DBus.ObjectManager" :member "InterfacesAdded")

      (labels ((devicep (device-path)
                 (cl-ppcre:scan "/org/freedesktop/UDisks2/block_devices" device-path))

               (new-device (device-path)
                 (format t "Trying new device ~a~%" device-path)
                 (when (and (devicep device-path)
                            (mountablep bus device-path))
                   (notify-new-device bus device-path)))

               (removed-device (device-path)
                 (notify-disconnected-device device-path)))

        (loop do
             (setf message (dbus::wait-for-incoming-message (dbus:bus-connection bus) '(dbus:signal-message) ))
             (when (typep message 'dbus:signal-message)
               (let ((event (dbus:message-member message))
                     (maybe-device (car (dbus:message-body message))))
                 (cond
                   ((string-equal event "InterfacesRemoved") (removed-device maybe-device))
                   ((string-equal event "InterfacesAdded")   (new-device maybe-device))
                   (t nil)))))))))

;;Actions
(defun mount-device (dev-info)
  (format nil "Mounted at ^B^[^3*~s^]"
          (handler-case (dbus:with-open-bus (bus (dbus:system-server-addresses))
                          (udisks:mount bus (getf dev-info :object)))
            (error () (error "^B^[^1*Error^] mounting ^B^[^3*~s^]" (getf dev-info :name))))))

(defun open-device (dev-info)
  (let ((path 
         (dbus:with-open-bus (bus (dbus:system-server-addresses))
           (car (udisks:get-mount-points bus (getf dev-info :object))))))
    (stumpwm:run-shell-command (format nil *dir-open-command* path))))

(defun mount-and-open (dev-info)
  (prog1 (mount-device dev-info)
    (open-device dev-info)))

(defun unmount-device (dev-info)
  (stumpwm:message "Unmounting ^B^[^1*~s^]" (getf dev-info :name))

  (handler-case (progn 
                  (dbus:with-open-bus (bus (dbus:system-server-addresses))
                    (udisks:unmount bus (getf dev-info :object)))
                  (format nil "Unmounted ^B^[^5*~s^]" (getf dev-info :name)))
    (error () (error "^B^[^1*Error^] Unmounting ^B^[^3*~s^]" (getf dev-info :name)))))


;;Manager helpers

(defvar *udisks-thread-name* "Stumpwm UDisks main loop")

(defun udisks-manager-launch ()
  "Starts a new thread for monitoring changes in udisks."
  (let ((thread (find *udisks-thread-name* (bordeaux-threads:all-threads) 
                :key #'bordeaux-threads:thread-name)))
  (when thread
    (stumpwm:message "Udisks manager already running: Restarting...")
    (bordeaux-threads:destroy-thread thread))
  (bordeaux-threads:make-thread (lambda () (handler-case (main-loop)
                                             (error () (stumpwm:message "Closing Udisks manager"))))
                                :name *udisks-thread-name*)))
