using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Configuration;
using AVRRuntime = ASNA.VisualRPG.Runtime;
using ASNA.DataGate.Client;
using ASNA.DataGate.DataLink;
using ASNA.DataGate.Common;

namespace MrLogic
{
    public partial class MobileRpgJob : ASNA.Monarch.WebJob
    {

        private bool promptLogon(LogonInfo logonInfo)
        {
            ASNA.Monarch.DynamicDisplayFile wfSignon = new ASNA.Monarch.DynamicDisplayFile("~/Monarch/SignOn.aspx");
            wfSignon.Open();

            char[] myIndicators = new char[100];
            for (int i = 1; i < 100; i++)
                myIndicators[i] = '1';
            if (logonInfo.PromptServer)
            {
                myIndicators[30] = '0';
            }

            logonInfo.Password = "";
            wfSignon.Write("RSIGNON", myIndicators, populateSignonBuffer, logonInfo);
            wfSignon.Read();
            if (wfSignon.FeedbackAID == (byte)ASNA.Monarch.WebDspF.AidKeyIBM.F3)
                return false;

            System.Data.DataRow row = wfSignon.DataSet.Tables["RSIGNON"].Rows[0];

            if (logonInfo.PromptServer)
            {
                logonInfo.Server = row["SYSTEM"].ToString().Trim();
                logonInfo.Port = (int)decimal.Parse(row["PORT"].ToString());
            }
            logonInfo.User = row["USER"].ToString().Trim();
            logonInfo.Password = row["PASSWORD"].ToString().Trim();
            logonInfo.Message = "";

            wfSignon.Close();

            return true;
        }

        private void populateSignonBuffer(string formatName, System.Data.DataRow newRow, object cookie)
        {
            LogonInfo logonInfo = cookie as LogonInfo;

            newRow["SYSTEM"] = logonInfo.Server;
            newRow["PORT"] = logonInfo.Port;
            newRow["USER"] = logonInfo.User;
            newRow["PASSWORD"] = logonInfo.Password;
            newRow["MESSAGE"] = logonInfo.Message;
        }

        private bool connect(string message)
        {
            LogonInfo logonInfo = new LogonInfo();
            logonInfo.Message = message;

            var webConfig = ConfigurationManager.AppSettings;
            var dbName = webConfig["MobileRPGDatabaseName"] ?? "";
            var hasDbName = !string.IsNullOrWhiteSpace(dbName);
            string resetExpiredPasswordStr = webConfig["ResetExpiredPassword"] ?? "false";
            bool resetExpiredPassword = resetExpiredPasswordStr.Equals("true", StringComparison.InvariantCultureIgnoreCase);


            myDatabase = new AVRRuntime.Database(dbName, AVRRuntime.VirtualTerminal.None, AVRRuntime.OpenAccessDspF.MobileRPG);
            while (true)
            {
                if (!hasDbName && !promptLogon(logonInfo))
                    return false;
                if (!hasDbName)
                {
                    myDatabase.Server = logonInfo.Server;
                    myDatabase.Port = logonInfo.Port;
                    myDatabase.User = logonInfo.User;
                    myDatabase.Password = logonInfo.Password;
                }
                try
                {
                    myDatabase.Open();
                    break;
                }
                catch (dgException dgEx)
                {
                    logonInfo.Message = dgEx.Message;
                    hasDbName = false;
                    if (dgEx.Error == dgErrorNumber.dgEsAS400ERROR && dgEx.ErrorClass == dgErrorClass.dgEC_AS400CPF && dgEx.SystemError == 0x22E4)
                    {
                        if (resetExpiredPassword)
                        {
                            if (!resetPassword(logonInfo))
                                return false;
                        }
                    }

                }
            }

            PsdsJobUser = myDatabase.User;
            return true;
        }

        #region ResetPassword
        private bool resetPassword(LogonInfo logonInfo)
        {
            ASNA.Monarch.DynamicDisplayFile wfSignon = new ASNA.Monarch.DynamicDisplayFile("~/Monarch/SignOn.aspx");

            char[] myIndicators = new string('0', 100).ToCharArray();
            string currentPass = "";
            string newPass = "";
            string message = "";

            wfSignon.Open();
            do
            {
                wfSignon.Write("RNEWPASS", myIndicators, populatePassBuffer, message);
                wfSignon.Read();
                if (wfSignon.FeedbackAID == (byte)ASNA.Monarch.WebDspF.AidKeyIBM.F3)
                    return false;

                System.Data.DataRow row = wfSignon.DataSet.Tables["RNEWPASS"].Rows[0];
                currentPass = row["PASSWORD"].ToString().Trim();
                newPass = row["NEWPASS"].ToString().Trim();
                if (newPass == row["CONFIRM"].ToString().Trim())
                    break;
                message = "Confirmation Password is not identical to New Password";
            } while (true);
            wfSignon.Close();

            logonInfo.Message = setNewPassword(logonInfo.User, currentPass, newPass);
            logonInfo.Password = newPass;
            return true;
        }

        void populatePassBuffer(string formatName, System.Data.DataRow newRow, object cookie)
        {
            newRow["PASSWORD"] = "";
            newRow["NEWPASS"] = "";
            newRow["CONFIRM"] = "";
            newRow["MESSAGE"] = cookie as string;
        }

        private string setNewPassword(string user, string password, string newPass)
        {
            AdgConnection reseterConnection;
            try
            {
                reseterConnection = new AdgConnection("*PUBLIC/PassReseter");
                reseterConnection.Open();
            }
            catch (Exception e)
            {
                return e.Message;
            }

            string result = string.Empty;
            string programName = "RESETPW";
            ProgParm userParm = new ProgParm(new ProgParmType("UserName", 0, FieldType.NewChar(10)), DataDirection.InputOutput);
            ProgParm oldPassParm = new ProgParm(new ProgParmType("OldPass", 0, FieldType.NewChar(128)), DataDirection.InputOutput);
            ProgParm oldLenParm = new ProgParm(new ProgParmType("OldLen", 0, FieldType.NewInteger(4)), DataDirection.InputOutput);
            ProgParm newPassParm = new ProgParm(new ProgParmType("NewPass", 0, FieldType.NewChar(128)), DataDirection.InputOutput);
            ProgParm newLenParm = new ProgParm(new ProgParmType("NewLen", 0, FieldType.NewInteger(4)), DataDirection.InputOutput);
            ProgParm resultParm = new ProgParm(new ProgParmType("Result", 0, FieldType.NewChar(100)), DataDirection.InputOutput);
            As400Program execPgm = new As400Program(reseterConnection, programName);
            execPgm.AppendParm(userParm);
            execPgm.AppendParm(oldPassParm);
            execPgm.AppendParm(oldLenParm);
            execPgm.AppendParm(newPassParm);
            execPgm.AppendParm(newLenParm);
            execPgm.AppendParm(resultParm);
            execPgm.ObjectToParm(userParm, user as object);
            execPgm.ObjectToParm(oldPassParm, password as object);
            execPgm.ObjectToParm(oldLenParm, password.Length as object);
            execPgm.ObjectToParm(newPassParm, newPass as object);
            execPgm.ObjectToParm(newLenParm, newPass.Length as object);
            execPgm.ObjectToParm(resultParm, result as object);
            try
            {
                execPgm.Execute();
                result = execPgm.ParmToObject(resultParm, typeof(string)) as string;
            }
            catch (dgException dgEx)
            {
                result = dgEx.Message;
            }
            reseterConnection.Close();
            return result;
        }
        #endregion
    }
}