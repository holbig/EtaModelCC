tabPanel("AboutModels",
         h2("About Models"),
         HTML('
		<p style="text-align:justify">The climate scenarios were generated from the Eta regional climate 
              model (<a href="http://dx.doi.org/10.1007/s00703-012-0182-z" target="_blank">Mesinger et al., 2012</a>) 
              configured in the 20 km resolution, in the area that covers 
              South America, Central America and the Caribbean 
              (<a href="http://dx.doi.org/10.4236/ajcc.2014.35043" target="_blank">Chou et al., 2014a</a>.
              <a href="http://dx.doi.org/10.4236/ajcc.2014.35039" target="_blank">Chou et al., 2014b</a>). 
              <br><br>
              The area covered by the model at a resolution of 5 km is reduced, due to the greater computational
              demand at this resolution. Downscaling was produced from global climate models, 
              HadGEM2-ES (<a href="https://doi.org/10.5194/gmd-4-1051-2011" target="_blank">Collins et al. 2011</a>); 
              MIROC5 (<a href="https://doi.org/10.1175/2010JCLI3679.1" target="_blank">Watanabe et al., 2010</a>), 
              CanESM2 (<a href="https://doi.org/10.1029/2010GL046270" target="_blank">Arora et al., 2011</a>) 
              and BESM (<a href="https://doi.org/10.1175/JCLI-D-12-00580.1" target="_blank">Nobre et al., 2013</a>). 
              <br><br>
              Historical period data, 
              also called a baseline or reference, correspond to the period in which concentrations 
              of greenhouse gases at current values are used. This period runs from 1961 to 2005. 
              The current climate change projections used the scenarios RCP4.5 and RCP8.5, for 
              the period from 2006 to 2100. 
              <br><br>
              Therefore, the climatic scenarios in the resolution of 
              20 km are composed of 4 simulations of the reference climate (historical), or 
              baseline, and 8 climate change projections (regionalization of 4 global models 
              with 2 emission levels). It is recommended to use the largest possible set of data 
              to better reflect the uncertainties associated with numerical modeling and the 
              greenhouse gas emission scenarios.</p>'),
value="AboutModels",
)
