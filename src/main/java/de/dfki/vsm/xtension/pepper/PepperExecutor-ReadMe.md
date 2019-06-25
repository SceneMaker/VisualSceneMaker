# PepperExecutor ReadMe

As of writing this, using Pepper in VSM requires the following:



- Python 2.7 32-bit
  - Available [here](https://www.python.org/downloads/release/python-2716/) ([download](https://www.python.org/ftp/python/2.7.16/python-2.7.16.msi))
- NaoQi Python SDK
  - Available [here](https://community.ald.softbankrobotics.com/en/resources/software/language/en-gb) (download requires a free SoftBank account)
- make the NaoQi SDK accessible for Python
  - Add SDK's `lib` subdirectory to your user's PYTHONPATH environment variable
    - On Windows 10: 
      - `Edit the system environment variables` -> `Environment Variables` :: `User variables for <you>`
      - If there is no variable named `PYTHONPATH`, then click `New...` and create it with the value `C:\path\to\sdk\pynaoqi-python2.7-2.5.5.5-win32-vs2013\lib` (replace with correct path)
      - Otherwise, append the correct value to your variable (separated by `;`)



Now set the correct plugin properties (description there) in the VSM editor to get started.