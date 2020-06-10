# Voltron

## TODO

- Mockup in Scotty?
- Or should we try a direct BINAH port?

### Views

- [x] Create 6 hardwired groups
- [x] Rejigger plain ace.html to use the vue.js
  - vue-ace-demo.html
- [x] Array of Vue/ace editors
  - voltron-0.html
- [x] Array of Vue/Firepad/ace editors
- [x] `instructor.html` should show *all* 6 panes
- [x] `student.html` should show pane for *group* 2 

### Routes

- [] `/course/:id/admin`
    - [] adding   groups    [also creates the firebase hash]
    - [] deleting groups    
    - [] adding   students  [SID, ] 
    - [] assign   groups

## Scotty 101

Build an authenticated "TODO" in scotty

- https://stackoverflow.com/questions/51070009/haskell-scotty-and-persistence-rest-api
- https://ro-che.info/articles/2016-04-14-scotty-http-basic-auth

Model 

```
User
  name       Text 
  email      Text 
  password   Bytestring

Todo
  user       User
  title      Text
  descr      Text
```



### Models

User
  email      Text 
  name       Text 

Class
  instructor User   -- only writable by admin
  name       Text   -- only writable by instructor

Group
  class      Class  -- only writable by instructor
  number     Int    -- only writable by instructor
  buffer     Text   -- only readable to class.instructor and students in group
                    -- only writable by instructor or admin

Enrolled
  user       User
  group      Group  -- only writable by instructor
