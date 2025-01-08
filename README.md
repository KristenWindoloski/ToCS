
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ToCS

## Description

ToCS is an in-development R package and web application that serves as a
graphical user interface (GUI) for high-throughput simulation of
chemical toxicokinetics (TK) and in vitro in vivo extrapolation (IVIVE).
The GUI builds upon the EPA’s httk R package
(<https://cran.r-project.org/web/packages/httk/index.html>,
<https://www.jstatsoft.org/article/view/v079i04>), which utilizes
general mathematical models to make high-throughput toxicokinetic
predictions for chemicals with little available data. This GUI is an
easy-to-use computational toxicology tool to predict:

- Concentration-time profiles
  - Generates ADME time course data, plots concentration curves over
    time, and provides summary statistics such as time to maximal
    concentration (Tmax), maximal concentration (Cmax), and area under
    the curve (AUC) for all model compartments across each chemical
- Steady state concentrations
  - Produces a table and plot of the analytical steady state
    concentration for the desired concentration and tissue of all
    selected compounds as well as a table estimating the time (days) it
    takes to reach steady state behavior for all compounds
- In vitro in vivo extrapolation (IVIVE)
  - Generates a table and plot of OEDs (oral equivalent dose, the
    external dose needed to produce the internal bioactive
    concentration) for all simulated chemicals
- Parameter calculations
  - Calculates tables and plots of TK parameters including the
    half-life, total plasma clearance, elimination rate, volume of
    distribution, and partition coefficients of the selected chemicals

without the need to know R or any programming. While other GUIs have
been created that use httk (Integrated Chemical Environment (ICE),
<https://ice.ntp.niehs.nih.gov/>) and share similar outputs to those in
this GUI, ToCS is unique in that it:

- Offers computation of analytical steady state concentrations and oral
  equivalent doses (OEDs) for all model and other tissue compartments,
  not just plasma
- Calculates the area under the curve (AUC) and time to maximum
  concentration for all model compartments
- Provides estimates of TK parameters not only including half-lives but
  also total plasma clearances, volumes of distribution, elimination
  rates, and partition coefficients
- Estimates the number of days it takes for compounds to reach plasma
  steady state
- Allows for further customization of simulations beyond basic model
  parameters
- Utilizes the most recent httk version (2.4.0)
- Allows for non-uniform chemical exposure (dosing), which can be
  beneficial for users interested in food chemicals
- Offers simulations for dog, rabbit, and mouse species in addition to
  human and rats
- Includes the option to include in silico-generated parameters within
  httk if in vitro hepatic clearance, fraction unbound in plasma, and/or
  caco-2 membrane permeability is missing
- Offers the option to convert the nominal bioactive concentration to a
  free concentration in vitro for IVIVE simulations (recommended)
- Allows the user to declare the desired quantile of predicted OEDs and
  view all generated OED samples.

ToCS is a toxicokinetics application that can be used by scientists with
little to no modeling or programming backgrounds, and requires a small
amount of user input to generate basic simulations. If desired,
simulations can be highly customizable through the Advanced Parameters
tab in ToCS.

## Getting Started

There are several ways that users can access the ToCS GUI. If the user
selects options 1) or 3), then the user will also need to have the free
statistical computing language, R, installed on their computer
(<https://www.r-project.org/>). Although optional, users may also want
to install RStudio (<https://posit.co/download/rstudio-desktop/>) as a
more user-friendly programming environment for R code. The following are
the ways that the user can access the ToCS GUI:

1)  By downloading all of the files from the ToCS GitHub page and
    loading it as an R project into R or R Studio
    - Go to <https://github.com/KristenWindoloski/ToCS> and click the
      green *Code* button. If the user is familar with Git, then they
      can clone the ToCS Git repository to their local computer.
      Otherwise, the user should click *Download Zip*. Wherever the user
      saves the folder, they should right click on it and unzip the
      folder.

    - Open R or RStudio.

    - The ToCS package uses 15 other packages (listed under the
      DESCRIPTION file), which will need to be installed on the user’s
      local computer. To install package *X* listed under *Depends* or
      *Imports* in the DESCRIPTION file, type the following into the R
      command line (console)

      ``` r
      install.packages("X")
      ```

    - Change your working directory to the ToCS folder by typing the
      following into the R command line (console) (replacing “filepath”
      with your computer’s file path to the ToCS folder)

      ``` r
      setwd("filepath")
      ```

    - Open the ToCS project folder by clicking *File -\> Open Project
      -\> ToCS-main*.

    - Load all files in the project directory into R by pressing
      Ctrl+Shift+L or typing the following into the R command line
      (console)

      ``` r
      devtools::load_all(.)
      ```

    - To open the GUI, type the following into the R command line
      (console)

      ``` r
      ToCS()
      ```

<!-- -->

2.  Using the online application (currently unavailable)
3.  Through CRAN and R, if the package is available in CRAN (currently
    unavailable):
    - Open R or RStudio.

    - Type the following into the R command line:

      ``` r
      install.packages("ToCS")
      ```

    - Then, type the following into the R command line:

      ``` r
      ToCS()
      ```

Any of the three mathods above (if available) should result in the ToCS
interface appearing. If the user successfully opens the app, their
screen should look like the image below.

<br/><br/>

<figure>
<img src="vignettes/GUI_Introduction_GenParsPage.png"
style="width:100.0%" alt="The opening interface to the ToCS app." />
<figcaption aria-hidden="true">The opening interface to the ToCS
app.</figcaption>
</figure>

Once in the interface, the user fills out any drop downs, text boxes, or
other selections on each of the five main tabs at the top of the GUI
moving left to right. Once the user has completed all of their
selections, they will run the simulation and receive the results on the
final tab, *Run Simulation*.

The section below provides a starting example for users to get familiar
with the ToCS software. Additional examples for each major output module
of the GUI can be found in the ToCS vignettes
(<https://github.com/KristenWindoloski/ToCS/tree/main/vignettes>).

## Concentration-Time Profile Example

Let’s say we want to run a simulation that outputs human
concentration-time profiles over the course of one day for four
compounds: Abamectin (CAS: 71751-41-2), Bisphenol-A (CAS: 80-05-7),
Cyanazine (CAS: 21725-46-2), and Dimethoate (CAS: 60-51-5). The
simulation will be for a single 5 mg/kg oral exposure of each compound
and use the PBTK model without including in silico generated parameters
in place of in vitro data (hepatic clearance, fraction unbound in
plasma, and caco-2 permeability).

#### General Parameters Tab

Since the main output we want is concentration-time profiles, we select
*Concentration-time profiles* from the drop down menu under the *Output*
card. Under the *Species* card, we select *Human* species and, while it
does not matter in this scenario what we select for the second drop down
since we selected human species, we select *Yes* to use human in vitro
data. We could also select *No* for human in vitro data as well and get
the same output. Thus, the first tab should look like the page below.

<br/><br/>

<figure>
<img src="vignettes/GUI_CTP_GenParsPage.png" style="width:100.0%"
alt="A completed opening interface to the ToCS app." />
<figcaption aria-hidden="true">A completed opening interface to the ToCS
app.</figcaption>
</figure>

<br/><br/>

Now, we move on to the *Model Specifications* tab.

#### Model Specifications Tab

On the *Dosing* card, we leave the first two drop down menus as their
default values. For the dosing frequency, we select *Single Dose* from
the drop down menu. This prompts the appearance of a textbox where we
can input the number of mg/kg to be administered. We change its value to
5 since we want a single 5 mg/kg exposure. On the *Model* card, we
select *pbtk* for the pbtk model on the first drop down menu. Since we
do not want to use in silico generated parameters for this simulation,
we select *No* for the second drop down menu under the *Model* card.
Finally, since we only want to run our simulation for one day, we edit
the bottom box in the *Model* card to be 1 instead of the default value
of 10. Now the *Model Specifications* tab is completed and should look
like the image below, so we can proceed to the *Compound Selection* tab.

<br/><br/>

<figure>
<img src="vignettes/GUI_CTP_ModelSpecEx1.png" style="width:100.0%"
alt="The completed model specifications tab for the pbtk model with a single oral dose of 5 mg/kg." />
<figcaption aria-hidden="true">The completed model specifications tab
for the pbtk model with a single oral dose of 5 mg/kg.</figcaption>
</figure>

<br/><br/>

#### Compound Selection Tab

Since we want to simulate four compounds (abamectin, bisphenol-a,
cyanazine, and dimethoate), we try searching the drop down menu under
the *Preloaded Compounds* card to see if the program is able to simulate
those chemicals with the current data in httk. To see the available
compounds, click on the empty box in the center column. We can see
already from the drop down menu in the image below that Abamectin is
available, so we click that compound. Remember that the user can search
for a compound by either its CAS number or chemical name.

<br/><br/>

<figure>
<img src="vignettes/GUI_CTP_CompoundSelectEx1.png" style="width:100.0%"
alt="The compound selection tab’s list of available preloaded compounds based on previous user selections." />
<figcaption aria-hidden="true">The compound selection tab’s list of
available preloaded compounds based on previous user
selections.</figcaption>
</figure>

<br/><br/>

By either scrolling or typing in the textbox, we see that the names of
the other three compounds are also available, so we select those. Since
all of the compounds we need are available, we do not need to upload a
CSV file under the *Uploaded Compounds* card and leave it untouched. So,
we hit the *Load Compounds* button under the *Instructions* card as
shown in the image below before proceeding to the next tab.

<br/><br/>

<figure>
<img src="vignettes/GUI_CTP_CompoundSelectEx1_Pic2.png"
style="width:100.0%"
alt="The completed compound selection card for example 1." />
<figcaption aria-hidden="true">The completed compound selection card for
example 1.</figcaption>
</figure>

<br/><br/>

#### Advanced (Optional) Parameters Tab

For simplicity of this example, we will leave all selections and inputs
on this tab alone and proceed to the next and final tab.

#### Run Simulation Tab

All input selections are complete and the correct compounds appear under
the *Selected Compounds* card, as shown in the image below. Therefore,
we hit the *Run Simulation* button under the *Actions* card so ToCS can
compute the solution. The output will appear in the *Results* window
when complete. Depending on the number of compounds selected to
simulate, the results may take several seconds to populate.

<br/><br/>

<figure>
<img src="vignettes/GUI_CTP_RunSimEx1_1.png" style="width:100.0%"
alt="The run simulations tab appearance before the “Run Simulation” button under the Actions card is clicked." />
<figcaption aria-hidden="true">The run simulations tab appearance before
the “Run Simulation” button under the <em>Actions</em> card is
clicked.</figcaption>
</figure>

<br/><br/>

The image below shows the first drop down in the *Results* card once the
simulation is complete. The user sees the complete time course curves of
all four chemicals in each model compartments overlaying each other. The
legend for the figure is located in the bottom right corner, and a
figure description describing the y-axis of each subplot is located
below the figure. The user also has the option to download this figure
by clicking *Download Figure 1*.

<br/><br/>

<figure>
<img src="vignettes/GUI_CTP_RunSimEx1_2.png" style="width:100.0%"
alt="The multi-curve plot output and download option for example 1." />
<figcaption aria-hidden="true">The multi-curve plot output and download
option for example 1.</figcaption>
</figure>

<br/><br/>

The second drop down in the *Results* card, as seen below, shows the
user the same plots as seen in the first drop down tab but with each
compound on a separate plot. The user has the option to download all
individual plots as a zip file. A figure caption is also located under
the very last plot in this tab.

<br/><br/>

<figure>
<img src="vignettes/GUI_CTP_RunSimEx1_3.png" style="width:100.0%"
alt="The individual plots output and download option for example 1." />
<figcaption aria-hidden="true">The individual plots output and download
option for example 1.</figcaption>
</figure>

<br/><br/>

The third drop down in the *Results* card allows the user to download
the time course simulation data that was used to generate the plots in
the two drop downs above. The user can also download all of the inputted
simulation parameters as well as the chemical data used in the
simulations. The interface with these two download buttons in shown
below.

<br/><br/>

<figure>
<img src="vignettes/GUI_CTP_RunSimEx1_4.png" style="width:100.0%"
alt="The simulation data download feature for example 1." />
<figcaption aria-hidden="true">The simulation data download feature for
example 1.</figcaption>
</figure>

<br/><br/>

Opening the bottom drop down in the *Results* card shows a toxicokinetic
summary including the Tmax (time to maximal concentration), Cmax
(maximal concentration), and AUC (area under the curve) of all simulated
compounds within each model compartment. The table is available for
download if the user clicks *Download Table 1*.

<br/><br/>

<figure>
<img src="vignettes/GUI_CTP_RunSimEx1_5.png" style="width:100.0%"
alt="The toxicokinetic (TK) summary drop down table for example 1." />
<figcaption aria-hidden="true">The toxicokinetic (TK) summary drop down
table for example 1.</figcaption>
</figure>

<br/><br/>

If the user wanted to run another simulation, it is recommended to click
the *Reset Session* button under the *Actions* card, which would clear
all parameter inputs and simulations and return the interface to the
*General Parameters* tab.

<br/><br/>

### Getting Help

The vignettes provided are intended to be user guides on how to run the
GUI. It is strongly recommended to consult them before running a
simulation. Each remaining vignette works through several examples for
each output module. To view the vignettes prior to ToCS being available
on CRAN or when using the online application, visit the GitHub page
(github.com/KristenWindoloski/ToCS/tree/main/vignettes) and click any of
the PDF vignette files. Once the ToCS package is available in CRAN, type

``` r
vignette(package = "ToCS")
```

into the R console to view the vignettes in R or RStudio or visit the
ToCS CRAN page.

If a user runs into an error, either gray or red text will appear under
the *Compound Selection* or *Run Simulation* tabs. If gray error text
appears, then the user has typically forgotten to enter a selection or
click a button, so the user should follow the instructions of the error
statement. If red error text appears, then the user should report the
error to the ToCS GitHub page
(<https://github.com/KristenWindoloski/ToCS/issues>).

<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. -->
<!-- In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN. -->
