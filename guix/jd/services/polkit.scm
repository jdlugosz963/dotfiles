(define-module (jd services polkit)
  #:use-module (gnu services)
  #:use-module (gnu services dbus)
  #:use-module (gnu home services)
  #:use-module (guix gexp))

(define polkit-network-manager
  (file-union
   "polkit-wheel"
   `(("share/polkit-1/rules.d/50-org.freedesktop.NetworkManager.rules"
      ,(plain-file
        "50-org.freedesktop.NetworkManager.rules"
        "polkit.addRule(function(action, subject) {
  if (action.id.indexOf(\"org.freedesktop.NetworkManager.\") == 0 &&
      subject.isInGroup(\"netdev\")) {
      return polkit.Result.YES;
  }
});")))))

(define-public polkit-network-manager-service
  (simple-service 'polkit-network-manager polkit-service-type (list polkit-network-manager)))
