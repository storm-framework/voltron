# voltron-server

## Models

User
  email      Text
  name       Text

Class
  instructor User   -- only writable by admin
  name       Text   -- only writable by instructor

Group
  class      Class  -- only writable by instructor
  number     Int    -- only writable by instructor
  editorLink Text   -- only readable to class.instructor and students in group
                    -- only writable by instructor or admin

Enroll
  user       User
  group      Group  -- only writable by instructor

## TODO

- [ ] ADMIN add classes from client
- [ ] use editor theme, keyBinds
- [ ] sync from instructor buffer

## Dependencies

You need to have stack in your `$PATH` for the following to work. 

If you want to edit `Model.storm` you'll also need 

- [storm-codegen](https://github.com/nilehmann/storm-codegen) 

see below for further instructions.

## Build the code

```
$ make build
```

## Run the server

The following will start the server in 127.0.0.0:3000

```
$ stack run
```

## Database Migrations

Persistent is not very smart out of the box to figure out how to run migrations. If you ever find an
error related to migrations when running the server removing the database should fix it.

```bash
$ rm db.sqlite
```

## Add instructor user

To add an instructor run

```
$ stack run -- add-instructor --email="email@domain.com" --password "password"
```

This will add an instructor without profile info. You can edit the profile in the app later.

## Note on editing `Model.storm`

If you edit `Model.storm` you'll first need to generate the corresponding `Model.hs`. 

The command `make build` is a wrapper over `stack build` that generates `Model.hs` from `Model.storm` when necessary. 

For this to work you need to have [`storm-codegen`](https://github.com/nilehmann/storm-codegen) in your `$PATH`.

If you don't modify `Model.storm` things should work just fine because `Model.hs` is under version control. 

Alternatively you could also run `make model` to generate `Model.hs`.
