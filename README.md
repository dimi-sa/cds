# cd on steroids

```console
foo@bar:~$ cds


docs -> /home/foo/Documents

desk -> /home/foo/Desktop

down -> /home/foo/Downloads


```

```console
foo@bar:~$ cds desk
foo@bar:~/Desktop$
```

```console
foo@bar:~$ cds help

To list nicknames:
cds

To add the nickname <nickname> for the working directory:
cds add <nickname>

To delete the nickname <nickname>:
cds del <nickname>

```

# Dependencies
## ghc
```
sudo apt install ghc
```

# Installation
```
make install
```

# Deletion
```
make uninstall
```

