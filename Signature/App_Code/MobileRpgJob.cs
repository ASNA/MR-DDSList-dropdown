using System;
using System.Text;
using System.Collections.Generic;
using System.Configuration;
using AVRRuntime = ASNA.VisualRPG.Runtime;
using ASNA.DataGate.Client;
using ASNA.DataGate.DataLink;
using ASNA.DataGate.Common;
using System.Linq;

namespace MrLogic
{
    public partial class MobileRpgJob : ASNA.Monarch.WebJob
    {
        private AVRRuntime.Database myDatabase;

        override protected AVRRuntime.Database getDatabase()
        {
            return myDatabase;
        }

        override public void Dispose( bool disposing )
        {
            if( disposing )
                myDatabase.Close();
            base.Dispose( disposing );
        }

        class LogonInfo
        {
            public string Server;
            public string User;
            public string Password;
            public string Message;
            public int Port;
            public bool PromptServer;

            public LogonInfo()
            {
                var webConfig = ConfigurationManager.AppSettings;
                Server = webConfig[ "MobileRPGServerName" ] ?? "cypress";
                User = webConfig[ "MobileRPGUsername" ] ?? "shakey";
                Password = "";
                Message = "";
                var portStr = webConfig[ "MobileRPGServerPort" ];
                if( portStr == null || !Int32.TryParse( webConfig[ "MobileRPGServerPort" ], out Port ) )
                    Port = 5150;
                string promptStr = webConfig[ "MobileRPGPromptForServer" ] ?? "no";
                PromptServer = !promptStr.Equals( "no", StringComparison.InvariantCultureIgnoreCase );
            }
        }


        override protected void ExecuteStartupProgram()
        {
            string message = "";
            try
            {
                if( !connect( message ) )
                    return;
            }
            catch( ASNA.Monarch.ShuttingDownException )
            {
                return;
            }

            try
            {
                var webConfig = ConfigurationManager.AppSettings;
                var libraryList = webConfig[ "MobileRPGLibraryList" ] ?? "";
                setupLibraryList( libraryList );
                var programName = webConfig[ "MobileRPGProgramName" ] ?? "HelloRPG";
                callProgram( programName );
            }
            catch( Exception e )
            {
                if( !( e is ASNA.Monarch.UnsupportedOperationException ) )
                    myDatabase.Close();
                var st = e.StackTrace.Replace( Environment.NewLine, "%0A" );
                this.ShowPage( string.Format( "~/Monarch/!Diagnose.aspx?m={0}&s={1}", e.Message, st ), null );
            }
        }

        private void setupLibraryList( string libraryList )
        {
            if( !string.IsNullOrWhiteSpace( libraryList ) )
            {
                var libraries = libraryList.Split( ',' ).Select( ( lib ) => { lib = lib.Trim(); return lib.StartsWith( "/" ) ? lib : "/" + lib; } ).ToArray();
                ILibraryList libraryListObject = AdgFactory.NewLibraryList( myDatabase.Connection );
                libraryListObject.CurrentUserLibs = libraries;
            }
        }

        private void callProgram( string programName )
        {
            As400Program program;
            program = new As400Program( myDatabase.Connection, programName );
            program.Execute();
        }
    }
}
