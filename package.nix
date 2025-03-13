{ pkgs ? import <nixpkgs> {}, displayrUtils }:

pkgs.rPackages.buildRPackage {
  name = "flipRevenueMetrics";
  version = displayrUtils.extractRVersion (builtins.readFile ./DESCRIPTION); 
  src = ./.;
  description = ''Revenue-based analytics for understanding the performance of subscription businesses.'';
  propagatedBuildInputs = with pkgs.rPackages; [ 
    reshape2
    flipTables
    ggplot2
    scales
    plyr
    verbs
    flipStatistics
    lubridate
    flipTime
    dplyr
    plotly
    flipStandardCharts
    flipFormat
  ];
}
