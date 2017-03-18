package com.zsibio

class Time extends Serializable{

  def formatTimeDiff(startTime: Long, finishTime: Long) : String = {
    val buf: StringBuffer = new StringBuffer();

    val timeDiff = finishTime - startTime;
    val hours = timeDiff / (60*60*1000);
    var rem = (timeDiff % (60*60*1000));
    val  minutes =  rem / (60*1000);
    rem = rem % (60*1000);
    val seconds = rem / 1000;

    if (hours != 0){
      buf.append(hours);
      buf.append("hrs, ");
    }
    if (minutes != 0){
      buf.append(minutes);
      buf.append("mins, ");
    }
    // return "0sec if no difference
    buf.append(seconds);
    buf.append("sec");
    return buf.toString();
  }

  def time[R](block: => R, method: String): R = {
    val t0 = System.currentTimeMillis()
    val result = block    // call-by-name
    val t1 = System.currentTimeMillis()
    println(method + ": " + formatTimeDiff(t0, t1))
    result
  }
}
