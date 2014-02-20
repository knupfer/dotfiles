;; org-mode
(add-to-list 'pretty-symbol-patterns '(?α kdm-custom "#\\+BEGIN_SRC" (org-mode)))
(add-to-list 'pretty-symbol-patterns '(?β kdm-custom "#\\+END_SRC" (org-mode)))
(add-to-list 'pretty-symbol-patterns '(?> kdm-custom "- \\[ ]" (org-mode)))
(add-to-list 'pretty-symbol-patterns '(?+ kdm-custom "- \\[-]" (org-mode)))
(add-to-list 'pretty-symbol-patterns '(?▷ kdm-custom "- \\[X]" (org-mode)))
(add-to-list 'pretty-symbol-patterns '(?δ kdm-custom "#\\+BEGIN_COMMENT" (org-mode)))
(add-to-list 'pretty-symbol-patterns '(?ξ kdm-custom "#\\+END_COMMENT" (org-mode)))
(add-to-list 'pretty-symbol-patterns '(?▶ kdm-custom "\\*\\*? TO\\|\\*\\*? DO" (org-mode)))
(add-to-list 'pretty-symbol-patterns '(?▶ kdm-custom "DO\\>\\|NE\\>" (org-mode)))
(add-to-list 'pretty-symbol-patterns '(?x kdm-custom "\\*\\*? FA\\|\\*\\*? CAN" (org-mode)))
(add-to-list 'pretty-symbol-patterns '(?x kdm-custom "ILED\\>\\|CELED\\>" (org-mode)))

;; Greek
(add-to-list 'pretty-symbol-patterns '(?α kdm-custom "\\<alpha\\>" (fundamental-mode inferior-python-mode text-mode prog-mode ess-mode)))  
(add-to-list 'pretty-symbol-patterns '(?Α kdm-custom "\\<Alpha\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))  
(add-to-list 'pretty-symbol-patterns '(?β kdm-custom "\\<beta\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))   
(add-to-list 'pretty-symbol-patterns '(?Β kdm-custom "\\<Beta\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))   
(add-to-list 'pretty-symbol-patterns '(?γ kdm-custom "\\<gamma\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))  
(add-to-list 'pretty-symbol-patterns '(?Γ kdm-custom "\\<Gamma\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))  
(add-to-list 'pretty-symbol-patterns '(?δ kdm-custom "\\<delta\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))  
(add-to-list 'pretty-symbol-patterns '(?Δ kdm-custom "\\<Delta\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))  
(add-to-list 'pretty-symbol-patterns '(?ε kdm-custom "\\<epsilon\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))
(add-to-list 'pretty-symbol-patterns '(?Ε kdm-custom "\\<Epsilon\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))
(add-to-list 'pretty-symbol-patterns '(?ζ kdm-custom "\\<zeta\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))   
(add-to-list 'pretty-symbol-patterns '(?Ζ kdm-custom "\\<Zeta\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))   
(add-to-list 'pretty-symbol-patterns '(?η kdm-custom "\\<eta\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))    
(add-to-list 'pretty-symbol-patterns '(?Η kdm-custom "\\<Eta\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))    
(add-to-list 'pretty-symbol-patterns '(?θ kdm-custom "\\<theta\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))  
(add-to-list 'pretty-symbol-patterns '(?Θ kdm-custom "\\<Theta\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))  
(add-to-list 'pretty-symbol-patterns '(?ι kdm-custom "\\<iota\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))   
(add-to-list 'pretty-symbol-patterns '(?Ι kdm-custom "\\<Iota\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))   
(add-to-list 'pretty-symbol-patterns '(?κ kdm-custom "\\<kappa\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))  
(add-to-list 'pretty-symbol-patterns '(?K kdm-custom "\\<Kappa\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))  
(add-to-list 'pretty-symbol-patterns '(?λ kdm-custom "\\<lambda\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode))) 
(add-to-list 'pretty-symbol-patterns '(?Λ kdm-custom "\\<Lambda\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode))) 
(add-to-list 'pretty-symbol-patterns '(?μ kdm-custom "\\<mu\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))     
(add-to-list 'pretty-symbol-patterns '(?Μ kdm-custom "\\<Mu\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))     
(add-to-list 'pretty-symbol-patterns '(?ν kdm-custom "\\<nu\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))     
(add-to-list 'pretty-symbol-patterns '(?Ν kdm-custom "\\<Nu\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))     
(add-to-list 'pretty-symbol-patterns '(?ν kdm-custom "\\<vega\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))   
(add-to-list 'pretty-symbol-patterns '(?ν kdm-custom "\\<Vega\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))   
(add-to-list 'pretty-symbol-patterns '(?ξ kdm-custom "\\<xi\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))     
(add-to-list 'pretty-symbol-patterns '(?Ξ kdm-custom "\\<Xi\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))     
(add-to-list 'pretty-symbol-patterns '(?ο kdm-custom "\\<omicron\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))
(add-to-list 'pretty-symbol-patterns '(?Ο kdm-custom "\\<Omicron\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))
(add-to-list 'pretty-symbol-patterns '(?π kdm-custom "\\<pi\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))     
(add-to-list 'pretty-symbol-patterns '(?Π kdm-custom "\\<Pi\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))     
(add-to-list 'pretty-symbol-patterns '(?ρ kdm-custom "\\<rho\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))    
(add-to-list 'pretty-symbol-patterns '(?Ρ kdm-custom "\\<Rho\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))    
(add-to-list 'pretty-symbol-patterns '(?σ kdm-custom "\\<sigma\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))  
(add-to-list 'pretty-symbol-patterns '(?Σ kdm-custom "\\<Sigma\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))  
(add-to-list 'pretty-symbol-patterns '(?τ kdm-custom "\\<tau\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))    
(add-to-list 'pretty-symbol-patterns '(?Τ kdm-custom "\\<Tau\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))    
(add-to-list 'pretty-symbol-patterns '(?υ kdm-custom "\\<upsilon\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))
(add-to-list 'pretty-symbol-patterns '(?Y kdm-custom "\\<Upsilon\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))
(add-to-list 'pretty-symbol-patterns '(?φ kdm-custom "\\<phi\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))    
(add-to-list 'pretty-symbol-patterns '(?Φ kdm-custom "\\<Phi\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))    
(add-to-list 'pretty-symbol-patterns '(?χ kdm-custom "\\<chi\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))    
(add-to-list 'pretty-symbol-patterns '(?Χ kdm-custom "\\<Chi\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))    
(add-to-list 'pretty-symbol-patterns '(?ψ kdm-custom "\\<psi\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))    
(add-to-list 'pretty-symbol-patterns '(?Ψ kdm-custom "\\<Psi\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))    
(add-to-list 'pretty-symbol-patterns '(?ω kdm-custom "\\<omega\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))  
(add-to-list 'pretty-symbol-patterns '(?Ω kdm-custom "\\<Omega\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))
