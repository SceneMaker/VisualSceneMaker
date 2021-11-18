# VLC Remote Controller - VSM Extension

This VSM extension allows the playback of any media supported by the VLC player.

How to use it:
* Run VLC player with the `rc` (remote console) interface.
  * See information at <https://wiki.videolan.org/Documentation:Advanced_Use_of_VLC/>;
  * For example, from the command line:

        path/to/VLC --extraintf rc --rc-host localhost:9000

* Create a VSM project and create a new device of type `VLCRemoteControllerExecutor`
  * Configure the properties:
    * `vlcport` - (int) The port VLC is listening to (e.g., 9000).
    * `mediarootdir` - (String, Optional) Set a root folder for searching the media files to play;
      * If NOT specified, the default media root directory is the same directory hosting the project dir.

    ![Example Configuration](images/DemoConfig.png)

* Add an agent with an "easy" name (e.g. 'vlc')
* Control media playback with the commands on the scenes. E.g.:
  * `vlc: [play file="path/to/file.mp4"].`
  * `vlc: [play url="http://path/to/file.mp4"].`
  * `vlc: [stop].`

![Example Configuration](images/DemoProject.png)



Advices:

* If you try to play a non existing file, or a corrupted file, VSM will give no errors. Check the VLC console for more information.
   