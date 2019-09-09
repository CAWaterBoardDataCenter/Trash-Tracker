## Welcome to the Trash Tracker Site
This site will serve as a repository for the [California State Water Resources Control Board's](https://www.waterboards.ca.gov/ "Water Board Homepage") Trash Tracker project, which is being led by the Board's Office of Information Management and Analysis, as well as catalog of related efforts around the state and beyond to better assess and analyze trash pollution and its effects. 

Through the Water Board's Trash Tracker project we aim to develop new methods to monitor and assess trash contained in locations that could potentially enter and impair waterbodies, using machine learning tools paired with large image libraries obtained from cameras deployed in key areas. We ultimately hope to develop a robust and accurate computer vision model, along with associated procedures and protocols for image collection and other aspects of study design, which will be provided on this site in a free and open-source format when completed and validated. The resulting model and data collection methods could then be applied wherever more data is needed to help mitigate water pollution caused by trash, and to demonstrate compliance with associated regulations.

Please check back in the future for updates and more information as the project progresses.

<p align="center">
	<img src="https://github.com/CAWaterBoardDataCenter/Trash-Tracker/blob/master/Misc/TrashTrackerMontage.png" width="60%" height="60%">
</p>

### Project Overview
The State Water Board’s Office of Information Management and Analysis is developing a new method to monitor and assess the quantity, distribution, and makeup of trash on streets to better understand the amount of trash entering (or being prevented from entering) California’s waterbodies. This method involves developing a customized computer vision model for trash identification, built on open-source machine learning resources and trained using a large library of images gathered from a street sweeper-mounted camera pointing at street curbs and gutters. The model and data collection method could then be deployed anywhere images are gathered in a similar manner, allowing interested parties to locate and quantify key sources of potential water pollution from trash.

The primary intent of this method is to provide municipalities with an efficient and cost-effective approach to monitor trash on their streets and subsequently direct best management practices to try to reduce the amount of trash entering the State’s waters. The method could ultimately provide an alternative for, or enhancement to, other methods used to determine compliance with new rules adopted by the State Water Board in 2015 (referred to as the Trash Plan Amendments), which require stricter monitoring and control of trash entering storm drain systems regulated by NPDES storm water permits. The motivation for this approach comes from a desire to create a rich dataset with detailed information that could inform management decisions and could be reassessed as new questions arise or technologies evolve, as well as a standardized and repeatable methodology that regulators can use to assess compliance in a highly transparent and consistent manner across regions and over time. 

Ultimately, the State Water Board must be able to tell a compelling story about the effectiveness and efficiency of the regulatory approaches used to reduce the amount of trash and debris entering our waters. Through this project, we are attempting to create a framework that will facilitate the development of the data and information needed to implement this vision. 

### Current Status
To develop and test this methodology, we have been working on a pilot project in partnership with the City of West Sacramento. For this project we collected images using cameras mounted to the front of the city’s street sweepers — which show the condition of the city’s streets prior to cleaning — and used the resulting image dataset to develop a proof-of-concept tool that identifies and classifies trash in images using a customized machine learning model. We are currently working to develop and verify a more robust and accurate model for trash identification and classification, along with more detailed procedures and protocols for image collection and other aspects of study design. The tools and procedures will be freely distributed through this site once completed. 

### Related Efforts
- [Code for Sacramento's Litter Detection with Microsoft Custom Vision](https://github.com/walteryu/code4sac/tree/master/custom-vision)
- [Collaborative Trash Data Model Development Project](https://github.com/CAWaterBoardDataCenter/TrashDataModel)
- [2018 Trash Data Dive Project Repositories](https://github.com/SCCWRP/2018TrashDataDive)
- [California Trash Monitoring Methods Project](https://sites.google.com/sfei.org/trash/)