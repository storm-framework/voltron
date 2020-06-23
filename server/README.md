# voltron-server

## TODO

1. Users and Roles

- [] "instructor-home" : list everyone by name
- [] "student-home"    : list me and just my info

2. Creating Groups

- [] "create-group"    : instructor creates groups
- [] "instructor-home" : list everyone by name + list all groups

3. Assigning Groups

- [] "assign-group"    : instructor assigns students to existing groups 
- [] "instructor-home" : list everyone by group
- [] "student-home"    : list everyone in my group 

4. Buffers

- [] "create-group"    : + instructor sets editor buffer for group
- [] "student-home"    : + show editor buffer
- [] "instructor-home" : + show ALL editor buffers 

## Dependencies

You need to have stack in your `$PATH` for the following to work. 

If you want to edit `Model.binah` you'll also need 

- [binah-codegen](https://github.com/nilehmann/binah-codegen) 

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

## Note on editing `Model.binah`

If you edit `Model.binah` you'll first need to generate the corresponding `Model.hs`. 

The command `make build` is a wrapper over `stack build` that generates `Model.hs` from `Model.binah` when necessary. 

For this to work you need to have [`binah-codegen`](https://github.com/nilehmann/binah-codegen) in your `$PATH`.

If you don't modify `Model.binah` things should work just fine because `Model.hs` is under version control. 

Alternatively you could also run `make model` to generate `Model.hs`.
