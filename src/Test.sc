import com.edawg878.bukkit.plot.PlotId

def auto(n: Int, x: Int, z: Int): (PlotId, Boolean) = {
  println(s"$x;$z")
  if (z < n) auto(n, x, z + 1)
  else if (x < n) auto(n, x + 1, -n)
  else auto(n + 1, -(n + 1), -(n + 1))
}

auto(0,0,0)