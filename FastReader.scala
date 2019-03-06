import java.io.BufferedReader
import java.io.IOException
import java.io.InputStreamReader
import java.util.StringTokenizer
class FastReader {
  var br: BufferedReader = new BufferedReader(new InputStreamReader(System.in))
  var st: StringTokenizer = _
  def next(): String = {
    while (st == null || !st.hasMoreElements()) try st = new StringTokenizer(
      br.readLine())
    catch {
      case e: IOException => e.printStackTrace()

    }
    st.nextToken()
  }
  def nextInt(): Int = java.lang.Integer.parseInt(next())
  def nextLong(): Long = java.lang.Long.parseLong(next())
  def nextDouble(): Double = java.lang.Double.parseDouble(next())
  def nextLine(): String = {
    var str: String = ""
    try str = br.readLine()
    catch {
      case e: IOException => e.printStackTrace()
    }
    str
  }

}

