# Executable Environment for OSF Project [42nyv](https://osf.io/42nyv/)

This repository was automatically generated as part of a project to test the reproducibility of open science projects hosted on the Open Science Framework (OSF).

**Project Title:** Young Infants' Attentional Preference for Social Interactions

**Project Description:**
> In this study we show that 9.5- to 11-month-old infants (n = 20), but not 7- to 8.5- month-olds (n = 20), look longer at videos showing two adults interacting with one another when simultaneously presented with a scene showing the same agents acting individually. Moreover, older infants showed higher social engagement during free play with their parent.  

Note: In the published study (https://doi.org/10.1111/cdev.13636), we refer to this project as “Experiment 1”. This OSF project is directly linked to the OSF project "Follow-up Study: Young infants' attentional preference for social interactions" (https://osf.io/s4uy7/). The supplemental information document contains information regarding both studies (referring to the two studies as "Experiment 1" and "Experiment 2"). In the follow-up OSF project, we provide data and scripts for additional analyses over a merged sample (combining the original sample from this project with the sample collected for the follow-up study). 

**Original OSF Page:** [https://osf.io/42nyv/](https://osf.io/42nyv/)

---

**Important Note:** The contents of the `42nyv_src` folder were cloned from the OSF project on **12-03-2025**. Any changes made to the original OSF project after this date will not be reflected in this repository.

The `DESCRIPTION` file was automatically added to make this project Binder-ready. For more information on how R-based OSF projects are containerized, please refer to the `osf-to-binder` GitHub repository: [https://github.com/Code-Inspect/osf-to-binder](https://github.com/Code-Inspect/osf-to-binder)

## flowR Integration

This version of the repository has the **[flowR Addin](https://github.com/flowr-analysis/rstudio-addin-flowr)** preinstalled. flowR allows visual design and execution of data analysis workflows within RStudio, supporting better reproducibility and modular analysis pipelines.

To use flowR, open the project in RStudio and go to `Addins` > `flowR`.

## How to Launch:

**Launch in your Browser:**

🚀 **MyBinder:** [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/code-inspect-binder/osf_42nyv-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment in your web browser.
   * Please note that Binder may take a few minutes to build the environment.

🚀 **NFDI JupyterHub:** [![NFDI](https://nfdi-jupyter.de/images/nfdi_badge.svg)](https://hub.nfdi-jupyter.de/r2d/gh/code-inspect-binder/osf_42nyv-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment on the NFDI JupyterHub platform.

**Access Downloaded Data:**
The downloaded data from the OSF project is located in the `42nyv_src` folder.

## Run via Docker for Long-Term Reproducibility

In addition to launching this project using Binder or NFDI JupyterHub, you can reproduce the environment locally using Docker. This is especially useful for long-term access, offline use, or high-performance computing environments.

### Pull the Docker Image

```bash
docker pull meet261/repo2docker-42nyv-f:latest
```

### Launch RStudio Server

Run the container (with a name, e.g. `rstudio-dev`):
```bash
docker run -it --name rstudio-dev --platform linux/amd64 -p 8888:8787 --user root meet261/repo2docker-42nyv-f bash
```

Inside the container, start RStudio Server with no authentication:
```bash
/usr/lib/rstudio-server/bin/rserver --www-port 8787 --auth-none=1
```

Then, open your browser and go to: [http://localhost:8888](http://localhost:8888)

> **Note:** If you're running the container on a remote server (e.g., via SSH), replace `localhost` with your server's IP address.
> For example: `http://<your-server-ip>:8888`

## Looking for the Base Version?

For the original Binder-ready repository **without flowR**, visit:
[osf_42nyv](https://github.com/code-inspect-binder/osf_42nyv)

