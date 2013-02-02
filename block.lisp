(in-package #:udisks)

(defconstant +block-devices-property-getters+ '((block-device           "Device"          #'bytes->string)
                                                (block-preferred-device "PreferredDevice" #'bytes->string)
                                                (block-symlinks         "Symlinks"        (lambda (aay) (mapcar #'bytes->string aay)))
                                                (block-device-number    "DeviceNumber")
                                                (block-id               "Id")
                                                (block-size             "Size")
                                                (block-read-only        "ReadOnly")
                                                (block-drive            "Drive")
                                                (block-md-raid          "MDRaid")
                                                (block-md-raid-member   "MDRaidMember")
                                                (block-id-usage         "IdUsage"         #'string->keyword)
                                                (block-id-type          "IdType"          #'string->keyword)
                                                (block-id-version       "IdVersion"       #'string->keyword)
                                                (block-id-label         "IdLabel")
                                                (block-id-uuid          "IdUUID")))


(defun enumerate-block-devices (bus)
  (let ((objects (mapcar #'car (dbus:get-managed-objects bus *udisks-service* "/org/freedesktop/UDisks2"))))
    (apply #'append
           (loop :for object :in objects
              :collect (if (cl-ppcre:scan "/org/freedesktop/UDisks2/block_devices/" object)
                           (list object))))))

(defun define-block-device-property-getter (fname pname &optional (transformation #'identity))
  (eval
   `(defun ,fname (bus block-device-object)
      (funcall ,transformation
               (dbus:get-property bus *udisks-service* block-device-object "org.freedesktop.UDisks2.Block" ,pname)))))

;;; Define all getters
(loop :for getter-spec :in +block-devices-property-getters+
   :do
   (apply #'define-block-device-property-getter getter-spec))


;;; Methods
(defun mount (bus block-device)
  (dbus:invoke-method (dbus:bus-connection bus)
                      "Mount"
                      :destination *udisks-service*
                      :path block-device
                      :interface "org.freedesktop.UDisks2.Filesystem"
                      :signature "a{sv}"
                      :arguments '(())))

(defun unmount (bus block-device)
  (dbus:invoke-method (dbus:bus-connection bus)
                      "Unmount"
                      :destination *udisks-service*
                      :path block-device
                      :interface "org.freedesktop.UDisks2.Filesystem"
                      :signature "a{sv}"
                      :arguments '(())))

(defun get-mount-points (bus block-device)
  (mapcar #'bytes->string
          (dbus:get-property bus *udisks-service* block-device "org.freedesktop.UDisks2.Filesystem" "MountPoints")))

