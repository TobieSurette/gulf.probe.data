# `gulf.probe.data` Package

Repository of Southern Gulf of Saint Lawrence active and passive monitoring probe data. These include VEMCO Minilog temperature and depth probes, Star Oddi depth-temperature and tilt probes, Scanmar, Netmind, eSonar and Notus trawl acoustic monitoring probes, as well as GPS track files.

# Data Sets:

Data sets are store in the `inst/extdata` directory. Data files are seperated by study year `XXXX`.

Directory            | Description
-------------------- | --------------------------------------------------
`scs.minilog.XXXX`   | Snow crab survey Minilog probe data. 
`scs.star.oddi.XXXX` | Snow crab survey Star Oddi probe data. 
`scs.scanmar.XXXX`   | Snow crab survey Scanmar acoustic monitoring probe data. 
`scs.netmind.XXXX`   | Snow crab survey Netmind acoustic monitoring probe data. 
`scs.esonar.XXXX`    | Snow crab survey eSonar acoustic monitoring probe data. 
`nss.notus.XXXX`     | Northumberland Starit survey Notus acoustic monitoring probe data. 

# Data Access:

Data can be accessed using the following function from the `gulf.data` package.

Function           | Description
------------------ | --------------------------------------------------
`read.minilog`     | Read Minilog probe data.
`read.star.oddi`   | Read Star Oddi probe data.
`read.scanmar`     | Read Scanmar acoustic trawl monitoring data.
`read.netmind`     | Read Netmind acoustic trawl monitoring data.
`read.esonar`      | Read eSonar acoustic trawl monitoring data.
`read.notus`       | Read Notus acoustic trawl monitoring data.
`read.gps`         | Read GPS track data.

