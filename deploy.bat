@echo off
setlocal enabledelayedexpansion

:: === DOCKER CONTAINER IMAGE AND PORT SETTINGS ===
:: A link the the github hosted docker image
set image=ghcr.io/open-aims/dh_sediment_monitoring:main
:: A port for the docker container
set container_port=3838
:: The location to mount the input data folder in the containter
set input_mount=/home/project


:: === CHECK DOCKER IS INSTALLED ===
set "docker_installed=false"
docker --version >nul 2>&1
if %errorlevel% equ 0 (set "docker_installed=true")
if %docker_installed%==false (echo ERROR: Docker not found. Ensure Docker Desktop is installed and that Docker is available from the command line via the `docker` command [e.g. in command prompt, try running: docker --version].) else ( echo Docker is installed)

:: === CHECK DOCKER IS RUNNING ===
set "docker_running=false"
docker info >nul 2>&1
if %errorlevel% equ 0 (set "docker_running=true")
if %docker_running%==false (echo ERROR: Docker doesnt appear to be running on your computer. Ensure Docker daemon is running by opening Docker Desktop.) else ( echo Docker is running)

:: === GET INPUT DATA FOLDER PATH FROM USER VIA GUI ===
:: Method from here: https://www.reddit.com/r/Batch/comments/gk6ucx/open_folder_selection_dialog/
set msg="Please select the input data folder."
set "psCommand="(new-object -COM 'Shell.Application').BrowseForFolder(0,'Please select input data folder',0,0).self.path""
for /f "usebackq delims=" %%I in (`powershell %psCommand%`) do set "input_folder=%%I"
echo Input data folder: %input_folder%

:: === GET DOCKER IMAGE FROM GITHUB ===
echo Pulling docker image from github - this might take a while...
docker pull %image%

:: === RUN DOCKER IMAGE AS CONTAINER ===
echo Running docker image as container...
set port=%container_port%:%container_port%
set volume=%input_folder%:%input_mount% 

:: Run docker continer in new background process 
set docker_run_call="docker run -it --rm -p %port% -v %volume% %image%"
echo %docker_run_call%
start "Darwin harbours Docker container" /B cmd /C %docker_run_call%

:: === OPEN THE SHINY APP IN BROWSER ===
start "Darwin harbours sediment monitoring dashboard" http://localhost:%container_port%

endlocal
