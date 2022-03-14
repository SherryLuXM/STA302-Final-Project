<div id="top"></div>


<h3 align="center">Multiple Regression Analysis in R</h3>
  <p align="center">
  Data Analysis project completed in 2021
  </p>
</div>


<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#roadmap">Roadmap</a></li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
    <li><a href="#acknowledgments">Acknowledgments</a></li>
  </ol>
</details>


## About The Project

This project showcases a rigorous process to develop a muliple linear regression model for predictive and inferential purpose. 
It uses a dataset of 1000 properties in California, with 14 variables, including the identifier and response. The goal is to understand the factors that affect a property's
housing price, and to make good prediction on the housing price given some new data with other information. The analytical process focuses on data description, 
data visualization, variable selection, model transformation, model diagnostics, model validation, and model interpretation. The concepts used include ANOVA, power 
transformation, analysis of covariance, AIC(Akaike Information Criteria), ajusted AIC, BIC, QQ plots, stepwise variable selection, VIF(variance inflation factor). The two 
preconditions and four assumptions for a multiple linear regression model are checked and the model is handled appropriately to deal with any violation. In the analysis of 
covariance, models with different main effect and interaction terms are investigated and compared. The variables have been checked for multicollinearity, and highly correlated 
variables and high leverage points are removed after taking the context into consideration.

### Built With
R Libraries:
* car
* MASS
* leaps
* pnag
* xtable
<p align="right">(<a href="#top">back to top</a>)</p>

<!-- USAGE EXAMPLES -->
## Usage

The procedure used in this project can be referenced for applying multiple linear regression in other datasets. Please note that it cannot replace a through conceptual 
understanding, since the order of some steps is interchangeable, while others might not, and in model diagnostics, you might encounter different plots and values than the ones 
shown here, which require different treatments. 

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- ROADMAP -->
## Roadmap

- [ ] Power Transformation 
- [ ] Preconditions Check
- [ ] Assumptions Check
- [ ] Variable Selection
- [ ] Analysis of Covariance
- [ ] High Leverage Points Check
- [ ] Preconditions Recheck
- [ ] Assumptions Recheck
- [ ] Model Validation
- [ ] Summary Statistics
    - [ ] Histogram
    - [ ] Boxplot
    - [ ] Scatterplot
    - [ ] Correlation table

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- CONTRIBUTING -->
## Contributing

Contributions are what make the open source community such an amazing place to learn, inspire, and create. Any contributions you make are **greatly appreciated**.

If you have a suggestion that would make this better, please fork the repo and create a pull request. You can also simply open an issue with the tag "enhancement".
Don't forget to give the project a star! Thanks again!

1. Fork the Project
2. Create your Feature Branch 
3. Commit your Changes
4. Push to the Branch 
5. Open a Pull Request

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- LICENSE -->
## License

Distributed under the MIT License. See `LICENSE.txt` for more information.

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- CONTACT -->
## Contact

Sherry Xiaoman Lu - sherry.luxiaoman@gmail.com
[![LinkedIn][linkedin-shield]][linkedin-url]

Project Link: [https://github.com/SherryLuXM/STA302-Final-Project](https://github.com/SherryLuXM/STA302-Final-Project)

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- ACKNOWLEDGMENTS -->
## Acknowledgments

* This is the final project from STA302/1001 Methods of Data Analysis 1, taught by Katherine Daignault, taken at University of Toronto in the 2020-2021 Winter term. Professor
Daignault has provided invaluable training and support. 

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- MARKDOWN LINKS & IMAGES -->
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://www.linkedin.com/in/sherry-l-633854132/