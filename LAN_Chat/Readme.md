# LAN Chat

LAN Chat is a simple chat programm that can be used within a LAN (or WAN if port forwarding to server is enabled)

Its main features are:
* chat with others (store chats if they are offline)
* rudimentary file transfer (only one file per client and time)
* some coloring features

! Attention !
This is a work in Progress project

![](preview.png)

### Icons

![](img_online.png) : The user is online and ready to chat

![](img_offline.png) : The user is offline, if you leave him a message it will pop up as soon as he connects

![](img_msg_pending.png): This user has send you a message which you did not read yet

### How to install

Dependencies:

- Bass ( https://www.un4seen.com/ )
- Lnet ( https://github.com/almindor/lnet )
  
Needed Lazarus Packages:

- UniqueInstance
- TurboPowerIPro

#### Server: 
- Compile and run the code on a machine that is always on
- The initial password for server administration is "**Default**" <br>
(Attention, its highly recommended to change this password, so that not every participant of the Chat group can access the server settings).

#### Client:
When starting the application the first time, the options dialog will pop up. You will need to set up at least the username, password, server ip and server port. Then click OK.


