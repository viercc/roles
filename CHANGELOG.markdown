0.3
---
* No longer supports GHC older than 8.6
* Add instances for many possible types in base
* Add instances for various monad transformers
* Instances that rely on 'eta' are back. Instead of 'new'
  (which depends on Newtype (type class) related machineries
  via 'lens'), employ manual labor.


0.2
---
* Drop lens, add containers dependency
* Remove instances that rely on 'new' and 'eta' (for now)


0.1
---
* Repository initialized
