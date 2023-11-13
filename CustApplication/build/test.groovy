@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import com.ibm.dbb.repository.*
import com.ibm.dbb.dependency.*
import com.ibm.dbb.build.*
import com.ibm.zos.sdsf.core.*

     println("Execute test jcl")
     def confDir = "/usr/lpp/IBM/dbb/conf"
     println("1")
     JCLExec jclExec = new JCLExec()
     try {
          println("2")
          jclExec.dataset("IBMUSER.TEST.JCL").member("TESTDBB").confDir(confDir).execute()
         }
      catch (NoClassDefFoundError ex) {
println("5")
          println("catching the No class def exception")
          def maxRC1 = jclExec.MaxRC()
          def jobID1 = jclExec.getSubmittedJobId()
          println("Newcopy of CICS program completed with RC - $maxRC1 Job ID is $jobID1")
println("6")
         }
println("7")
     println("Test JCL completed")
