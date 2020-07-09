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
  buffer     Text   -- only readable to class.instructor and students in group
                    -- only writable by instructor or admin

Enroll
  user       User
  group      Group  -- only writable by instructor


## TODO

- [ ] 'Enroll'
  
  - type EnrollStudent = { className: string, studentEmail: string, groupName: string } 

  - type Enroll = { newBuffers : Buffer[], newEnrolls: EnrollStudent[] }

  - FRONT
  	- [ ] 'Enroll' page to allow upload of CSV -> render into a JSON `Enroll` object
	- [ ] Generate an `Enroll` object as above
	- [ ] POST /api/enroll 

  - BACK 
    	- [] create GROUPs   if they don't exist
    	- [] create STUDENTs if they don't exist using email-user as password
    	- [] create ENROLLs  (if they don't exist)

- [ ] 'Settings'

- [ ] 'Reset Password'

## Populating the Database

```sh
$ stack exec -- voltron add-instructor --email=rjhala@eng.ucsd.edu --password=rjhala
$ stack exec -- voltron add-group --grpname=0 --editorlink=-M9Kx-cxRIUgCqVCtjCr
$ stack exec -- voltron add-group --grpname=1 --editorlink=-M9L5YBS0kgvUfuz0Ckc
$ stack exec -- voltron add-group --grpname=2 --editorlink=-M9L5oPt0fsruy16vntv
$ stack exec -- voltron add-group --grpname=3 --editorlink=-M9L5vCVa5FQ0noobA9V
$ stack exec -- voltron add-student --email=wkunkel@eng.ucsd.edu --password=rose --grpname=0
$ stack exec -- voltron add-student --email=nlehmann@eng.ucsd.edu --password=nico --grpname=0
$ stack exec -- voltron add-student --email=rkici@eng.ucsd.edu --password=rkici --grpname=1
```

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
