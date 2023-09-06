## Instructions

The goldeneye package is designed to facilitate easy encryption/decryption of potentially sensitive data.  This shiny app is designed to make some of the features of the package available to those that do not use R.  If you are a confident R user, then you can use the functions gy_save and gy_zip directly.

Otherwise, please follow the steps below to set up an encryption profile, encrypt files, and decrypt files.  You can return to these instructions at any time by clicking the "Instructions" tab above.

### Step 1:  Profile Setup

Click on the "Profile" tab to start.  

The first time you run this app you will need to set up a goldeneye profile (unless you already have a valid profile from previously using goldeneye/goldfinger encryption) - to do this, select "Create New" and enter the details requested.  Note that the password and re-typed password must match.  It is also VERY important that you do not forget your password!  Once you have entered your details you can click on "Create Profile".  You should then get a message that your profile was created successfully.  After this, you should click the link to download your new private key file.  Save this file in a safe place - you will need it next time you run the app.

If you already have a profile file, then you can click on the "Select Existing" button and then use the file browser to select your profile file.  If you have previously used the profile on the same computer then your password should be saved; otherwise you have to re-enter it.  Then click on the "Check Password" button to validate.

You also have the option to download a public key file from this tab.  If you want other people to send you encrypted data, then you need to send them this public key file for them to do so.  You can re-download your public key file at any time.


#### Encrypt File(s)

Once you have set up / selected your profile, you can begin to encrypt data.  Click on the Browse button to select one or more files to encrypt.  You should then see two sets of options appear:  a text entry box for online/web-based public keys, and another file browser for public keys on your computer.  These options control who else will be able to decrypt the files you encrypt. If you only want to encrypt data for your own use then you can ignore both of these inputs. There is also a check-box to allow decryption by yourself - typically you want to leave this ticked, but sometimes you might want to encrypt data so that you cannot decrypt it yourself later.

Using weblinks is convenient, but requires the other user to have put their public key online.  You can try an example of this by entering https://www.costmodds.org/people/matt/goldeneye_public.gyu - then click the "Add Weblink" button to process this key.  If you want to, you can then add more weblinks in the same way.

Using local public key files means that your collaborator just needs to send you their public key via e.g. email.  Click the Browse button to select the public key (you will first need to save this somewhere on your computer), then click the "Add File" button to process the key.  If you want to, you can then add more public key files in the same way.

Once you are happy with the file selection and encryption settings, then press the "Encrypt and Save" button.  This will encrypt the file(s) and provide you a single encrypted archive file that you can safely store and/or send to collaborators over potentially unsafe channels.


#### Direct entry

For direct data entry, you should first select the study type (paired or unpaired, aka case/control), and then select the number of animals.  After clicking on the "Initialise data entry" button you can then enter your data into the cells below.  All data entered must be valid numbers - if an observation is missing then leave the corresponding cell blank.

#### Demonstration

If you want to quickly see what the tool can do without having to enter data, then you can use the demonstration option.  This randomly generates some data (for a paired study with 20 animals), and pre-fills the other options you need to run the analysis.


### Step 2:  Select Parameters

Once you have entered data, you can select the relevant parameters for your analysis using the "Parameters" tab.  The first option to select is between the clinical and research version of the guidelines: the clinical version has a lower sample size requirement at the cost of a larger grey zone, while the research version has an improved minimum detectable resistance level at the cost of a larger sample size requirement. For more details of the difference between these see the 2023 WAAVP guideline for diagnosing anthelmintic resistance using the faecal egg count reduction test in ruminants, horses and swine (Kaplan et al, 2023). You should then enter the following additional parameters:

- Species and anthelmintic:  the host/parasite species and anthelmintic that you have used for your study. Note that this only covers the situations covered by Kaplan et al. (2023) - a more flexible option for specifying custom efficacy targets will be added soon.
- Multiplication factor:  the counting sensitivity of the laboratory method used (e.g. 50 for McMaster). This must be a number greater than zero, and will be used to divide the specified EPG before analysis (which must result in a whole number i.e. count for each data point). Multiplication factor may differ between pre- and post-treatment (or treatment and control), but it is currently not possible for multiplication factor to vary within each observation set (as may be needed for some automated FEC methods).
- Country/region:  an optional text entry giving the country/region where the animals are kept, which will be included in the report if specified.
- Study identifier:  an optional text entry giving the study ID for reference purposes, which will be included in the report if specified.


### Step 3:  Results

Once you have entered data and provided valid parameters, you can run the analysis and obtain your results using the "Results" tab. First, click on the "Click to Calculate" button (if this button is not shown, then you should instead follow the instructions to correct the problem with the data/parameters that you entered). After a short wait, you should then see the headline result of your analysis (for each dataset provided) appear. You can then select to download a full report (including more information such as summary statistics, and comparisons of results generated by different analysis methods), either in Word or PDF format.

If you wish, you may go back and change parameters and then re-run the analysis.  You can also edit your data if you have entered this directly, or replace the files uploaded with new datasets.  Due to technical reasons, it is not currently possible to change the data entry method to direct data entry (or demonstration) after a file has already been uploaded (but you can refresh your browser to re-load the tool if you need to do this).


### About

This online tool was developed by Matt Denwood at the University of Copenhagen, with input from Ray Kaplan, Martin K. Nielsen, Bruno Levecke, Stig Milan Thamsborg, and Iain McKendrick.  We acknowledge funding from NordForsk (the DigiVet project on digitalisation of livestock data:  https://www.dcs.gla.ac.uk/~jenright/digivet_website/) and the Scottish Government Rural and Environment Science and Analytical Services Division (RESAS) Strategic Research Programme, for support of this work.  If you have comments/questions/suggestions, then please feel free to get in touch [by emailing Matt Denwood](md@sund.ku.dk).

For more information on the WAAVP guidelines see [Kaplan et al. (2023)](https://pubmed.ncbi.nlm.nih.gov/37121092/).  To cite this tool in peer-reviewed publications (and for more details on the underlying statistical principles) see [Denwood et al. (2023)](https://pubmed.ncbi.nlm.nih.gov/36621042/).  A version of this tool is hosted at [the fecrt.com website](https://www.fecrt.com).

### Version

This software is part of the [open-source bayescount package](https://github.com/ku-awdc/bayescount), version 1.1.0 (2023-08-21).
