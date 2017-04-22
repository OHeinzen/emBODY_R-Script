# emBODY_R-Script
R-Script for processing and visualizing data of the [emBODY tool](https://git.becs.aalto.fi/eglerean/embody).

The emBODY tool was originally developed for this publication:  
Nummenmaa L., Glerean E., Hari R., Hietanen, J.K. (2014) Bodily maps of emotions, Proceedings of the National Academy of Sciences of United States of America [doi:10.1073/pnas.1321664111](http://www.pnas.org/content/111/2/646.abstract)

---

*emBODY_R-Script (c) 2017  Oliver Heinzen & Jörg Trojan*

This script is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

## Instructions
The script is separated into five steps. The first step involves data input and preprocessing. Step two through four offer different forms of data aggregation into respective, proportionate measures of activation and deactivation, specific to the assessed stimulus condition. Step five provides plotting examples, analogous to those used in the development of this script.

Each file starts off with a section requiring information to specify parameters of the respective script, like file paths, the number of stimuli used in the data or the coordinates of regions of interest. After having filled out the information interface of each script, the rest of the code can be run without any further interaction. This design strategy was chosen to provide users with the functionality of the script without the need to understand and recreate each little part of the computing process. A sample of three hypothetical subjects has been included to simulate the use of the script. At the end of each step, the current result of data processing and calculations is written to an output file.

For specific details regarding the workings of the script or the circumstances in which it was written, please see the method section of my thesis paper. The PDF is included in this project.

### Step One: Data input & reprocessing
This part creates two data frames (DF) within the R-environment. The first one, called "dfr", includes emBODY coordinates, time stamp, ID and a matching variable. The second one, called "embody", contains the demographic data recorded on the registration page of the emBODY tool. All subsequent processed and aggregated data will be stored in this DF.

It has to be noted, that data should be complete. It is not possible to load partial data with this script, if the emBODY tool did not record any markings for a respective stimulus condition. Thus, if file 0.csv or 3.csv are missing, an error will be returned signaling the script is not working. Please make sure to only include complete subjects.

**Required information:** 
* Path to your working directory
* Path to the subjects folder, preferably within your working directory 
* The number of stimuli used in your data (default and maximum is 14, as the tool was originally intended for 14 emotions)
* The names of stimuli used in your data
	* **Important:** The first index is 0, not 1, and names have to be separated by a semicolon. Also note, that this list needs to correspond with the order of stimuli you have arranged in the stimuli file of the emBODY tool. If this order is not implemented here, files will be mismatched.
* The names of the demographic variables
	* **Important:** The object needs to end on "NA". The tool falsely records a comma at the end of each "data.txt" file. Thus, the last variable is just being left empty and can be deleted after the embody DF has been created.

### Step Two: ROI-specific processing
In this step, markings of activation and deactivation are restricted to specified regions of interest (ROIs). For these areas, proportionate scores of respective activation or deactivation will be calculated and appended to the "embody" DF. ROIs are defined by two X and two Y coordinates, indicating vertical and horizontal lines which delimit the ROI. For more details, please see the method section of the thesis project. 

Given the original assessment of 14 emotions, 14 different ROI options were provided, one for each emotion. If required, the script can be used multiple times for more than 14 ROIs. The already specified ROIs represent the regions used in the development of this script.

During the aggregation of scores, markings will be extended by a brush disk. While the emBODY tool is filled out with a disk-shaped cursor, only the location at the center of this disk will be recorded. Thus, data around each of these locations will also set to be activated or deactivated in a 15-pixel wide diameter.

In order to define your own ROIs, you need to determine the mentioned coordinates. This script does not provide an easy way to do so and we acknowledge the lack of an accessible method. The most straightforward approach is to plot the masking image of the tool onto the 171 x 521 pixel grid, save this image in a resolution where 1 point equals one pixel and approximate the ROI in Microsoft Paint. Just be aware, that image software locates the Y-zero-point in the upper left corner, while coordinate systems use the lower left corner. Thus, Y-coordinates have to be inverted when ROIs are specified. For orientation, again plot the masking image on the mentioned grid and add straight lines to the plot at intended coordinate intercept points. This will help you approximate a ROI.

*Disclaimer:* Given the straightforward structuring of the data, the calculation of resulting scores can take up quite some time, especially with large sample sizes. 

**Required information:** 
* The number of ROIs to be assessed
* Specifying ROIs by inputting, in this sequence, the lower X-coordinate, the higher X-coordinate, the lower Y-coordinate, the higher Y-coordinate and the stimulus name, which needs to correspond to the name given in step one.

### Step Three: Extended ROI-specific processing
This step is fairly analogous to step two, only with the addition that ROIs are subtracted from each other. This was done in order to account for negative space between the legs, when we conducted our own investigation. While this step is relatively specific, we wanted to provide the according code for users potentially interested in using the same method. Specification of ROIs is the same as in step two, but ROI D and E will be subtracted from the merged ROIs of A, B and C. Lowercase letters represent the respective left and right parts of the legs. If a different set up of added and subtracted ROIs is required, the code after section 3.1 needs to be changed.

*Disclaimer:* Given the straightforward structuring of the data, the calculation of resulting scores can take up quite some time, especially with large sample sizes. 

**Required information:** 
* Specifying ROIs by inputting, in this sequence, the lower X-coordinate, the higher X-coordinate, the lower Y-coordinate, the higher Y-coordinate and the stimulus name, which needs to correspond to the name given in step one.
* A name specifier for the final ROI, in order to automatically append results after running the script

### Step Four: Whole body processing
In this step the restriction to ROIs is lifted and data for the whole body is processed. Given the large amount of time necessary for calculations, processing for activation and deactivation was split into two scripts. The script including a lowercase A calculates deactivation. While aggregated scores will be added to the "embody" object and written to the respective output file, unaggregated scores will be written to a different output file.

*Disclaimer:* Given the straightforward structuring of the data, the calculation of resulting scores can take up quite some time, especially with large sample sizes. This issue becomes more prominent without the use of ROI restrictions. In development, whole body processing for one stimulus at a sample size of about 260 participants took up to 36 hours. For several operations, we recommend running several instances of R on a computer with at least 8GB RAM.

**Required information:**
* The stimulus name, which needs to correspond to the name given in step one.

### Step Five: Plotting examples
In this last step, code is provided which was used in the analysis of the master thesis data. The unaggregated data is plotted, in what is essentially a 3D histogram, on a black background with frequencies depicted in hexagon bins. The bin size is a derivative of the grid aspect ratio. Two colored scale gradients were defined, representing activation and deactivation. Black to red to orange represents differing gradients of activation, while black to dark to light blue represent differing gradients of deactivation. We recommend a 344 x 625 pixel export resolution.

**Required information:**
* Unaggregated data of step four should be loaded into "body" object
* Adjust "limits" and "breaks" parameters to the magnitude of frequencies
	* *Note:* A general rule of thumb is to use the smallest possible maximum of frequencies. If the bin limitation does not exceed the largest amount of frequencies, the respective hexagon bin will be displayed in gray color. Thus, to depict the most useful range of scaling, set the limits at the smallest possible level, at which gray areas are not shown.