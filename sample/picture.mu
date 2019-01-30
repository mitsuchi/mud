type Picture = { 
  filename: String,
  lat: Double,
  lon: Double
}

skyPic = Picture 'sky.jpg' 1.0 2.0
skyPic.filename.puts

fun addLat : Picture -> Double -> Picture = p lat0 -> {
    Picture (p.filename) (p.lat+lat0) (p.lon)
}

(addLat skyPic 2.0).lat.puts