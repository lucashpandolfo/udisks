;;;; package.lisp

(defpackage #:udisks
  (:use #:cl)
  (:export 
   ;;Block
   #:enumerate-block-devices
   #:block-device          
   #:block-preferred-device
   #:block-symlinks        
   #:block-device-number   
   #:block-id              
   #:block-size            
   #:block-read-only       
   #:block-drive           
   #:block-md-raid         
   #:block-md-raid-member  
   #:block-id-usage        
   #:block-id-type         
   #:block-id-version      
   #:block-id-label        
   #:block-id-uuid
   
   #:mount
   #:unmount
   #:get-mount-points))
