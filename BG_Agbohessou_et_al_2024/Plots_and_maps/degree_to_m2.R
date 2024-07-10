# conversion fucntions for degree to m2
# y. agbohessou

# Conversion function for latitude
convert_deg_to_m_lat <- function(degrees) {
  return(degrees * 111320)  # Approximate conversion factor for WGS 84
}

# Conversion function for longitude
convert_deg_to_m_lon <- function(degrees, latitude) {
  return(degrees * (40075000 * cos(latitude * pi / 180) / 360))  # Approximate conversion factor for WGS 84
}

# Example usage
latitude <- 0.1 # Example latitude in decimal degrees (e.g., New York City)
longitude <- 0.1  # Example longitude in decimal degrees (e.g., New York City)

# Convert latitude to meters
lat_in_meters <- convert_deg_to_m_lat(latitude)
cat("Latitude in meters:", lat_in_meters, "\n")

# Convert longitude to meters
lon_in_meters <- convert_deg_to_m_lon(longitude, latitude)
cat("Longitude in meters:", lon_in_meters, "\n")
res_ha=(lat_in_meters*lon_in_meters)/10000
