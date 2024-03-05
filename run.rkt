'("listo! " (a b c . #(x)))             '("listo! " (a b c . #(x)))
'("nullo! " (a b c . #(x)))             '("nullo! " (a b c . #(x)))
'("cdro! "  (a b c . #(x)) #(x_0))      '("cdro! "  (a b c . #(x)) #(x_0))

'("listo! " #(x_0))                     '("listo! " #(x_0))
'("nullo! " #(x_0))                     '("nullo! " #(x_0))
'("cdro! "  #(x_0) #(x_0))              '("cdro! "  #(x_0) #(x_0))

'("listo! " #(x_0))                     '("listo! " #(x_0))
'("nullo! " #(x_0))                     '("nullo! " #(x_0))
'("cdro! "  #(x_0) #(x_0))              '("cdro! "  #(x_0) #(x_0))

'("listo! " #(x_0))                     '("listo! " #(x_0))
'("nullo! " #(x_0))                     '("nullo! " #(x_0))
'("cdro! "  #(x_0) #(x_0))'(())         '("cdro! "  #(x_0) #(x_0))

                                        '("listo! " #(x_0))
                                        '("nullo! " #(x_0))
                                        '("cdro! "  #(x_0) #(x_0))'(() (_0))



'("listo! " (a b c . #(x)))
'("nullo! " (a b c . #(x)))
  "made it"'("cdro! " (a b c . #(x)) #(x_0))"made it"
  
'("listo! " #(x_0))
'("nullo! " #(x_0))
"made it"'("cdro! " #(x_0) #(x_0))"made it"

'("listo! " #(x_0))
'("nullo! " #(x_0))
"made it"'("cdro! " #(x_0) #(x_0))"made it"

'("listo! " #(x_0))
'("nullo! " #(x_0))
"made it"'...(())


;; it seems like it must fail at the final cdro - maybe obligated to try
;; even after the null succeeds. but what about for run 2+?
;; is it still run in the same order? when it does fail, how does it
;; not fail on subsequent times?
