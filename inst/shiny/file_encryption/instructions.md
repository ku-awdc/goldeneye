## Instructions

The goldeneye package is designed to facilitate easy encryption/decryption of potentially sensitive data.  This shiny app is designed to make some of the features of the package available to those that do not use R.  If you are a confident R user, then you can use the functions gy_save and gy_zip directly.

Otherwise, please follow the steps below to set up an encryption profile, encrypt files, and decrypt files.  You can return to these instructions at any time by clicking the "Instructions" tab above.

### Step 1:  Profile Setup

Click on the "Profile" tab to start.  

The first time you run this app you will need to set up a goldeneye profile (unless you already have a valid profile from previously using goldeneye/goldfinger encryption) - to do this, select "Create New" and enter the details requested.  Note that the password and re-typed password must match.  It is also VERY important that you do not forget your password!  Once you have entered your details you can click on "Create Profile".  You should then get a message that your profile was created successfully.  After this, you should click the link to download your new private key file.  Save this file in a safe place - you will need it next time you run the app.

If you already have a profile file, then you can click on the "Select Existing" button and then use the file browser to select your profile file.  If you have previously used the profile on the same computer then your password should be saved; otherwise you have to re-enter it.  Then click on the "Check Password" button to validate.

You also have the option to download a public key file from this tab.  If you want other people to send you encrypted data, then you need to send them this public key file for them to do so.  You can re-download your public key file at any time.


### Step 2:  Encrypt File(s)

Once you have set up / selected your profile, you can begin to encrypt data.  Click on the Browse button to select one or more files to encrypt.  You should then see two sets of options appear:  a text entry box for online/web-based public keys, and another file browser for public keys on your computer.  These options control who else will be able to decrypt the files you encrypt. If you only want to encrypt data for your own use then you can ignore both of these inputs. There is also a check-box to allow decryption by yourself - typically you want to leave this ticked, but sometimes you might want to encrypt data so that you cannot decrypt it yourself later.

Using weblinks is convenient, but requires the other user to have put their public key online.  You can try an example of this by entering https://www.costmodds.org/people/matt/goldeneye_public.gyu - then click the "Add Weblink" button to process this key.  If you want to, you can then add more weblinks in the same way.

Using local public key files means that your collaborator just needs to send you their public key via e.g. email.  Click the Browse button to select the public key (you will first need to save this somewhere on your computer), then click the "Add File" button to process the key.  If you want to, you can then add more public key files in the same way.

Once you are happy with the file selection and encryption settings, then press the "Encrypt and Save" button.  This will encrypt the file(s) and provide you a single encrypted archive file that you can safely store and/or send to collaborators over potentially unsafe channels.


### Step 3:  Decrypt File(s)

The final step is to decrypt an encrypted data file.  Select the Decryot tab, then use the Browser button to select an encrypted file.  You can then choose the folder in which to save the decrypted files - either as a single file or as a zip archive, depending on the number of encryoted files saved.


### About

This online tool was developed with funding from the NordForsk-funded DigiVet project on digitalisation of livestock data:  https://www.dcs.gla.ac.uk/~jenright/digivet_website/.  If you have comments/questions/suggestions, then please feel free to get in touch [by emailing Matt Denwood](md@sund.ku.dk).


### Version

This software is part of the [open-source goldeneye package](https://github.com/ku-awdc/goldeneye), version 0.6.0 (2023-09-06).
