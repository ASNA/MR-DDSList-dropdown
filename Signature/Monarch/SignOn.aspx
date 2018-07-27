<%@ Page Language="C#" AutoEventWireup="true" CodeFile="SignOn.aspx.cs" Inherits="SignOn" MasterPageFile="~/Themes/Current/MasterPage.master" %>
<%@ Register TagPrefix="mdf" Assembly="ASNA.Monarch.WebDspF, Version=15.0.48.0, Culture=neutral, PublicKeyToken=71de708db13b26d3" Namespace="ASNA.Monarch.WebDspF" %>

<asp:Content ID="Content1" ContentPlaceHolderID="HeaderPH" runat="Server" >
</asp:Content>

    <asp:Content ID="FileContent2" runat="server" ContentPlaceHolderID="CenPH">
        <mdf:ddsfile id="SignOnControl" runat="server" DisplayErrorMessages="True" BannerStyle="Invisible"  />
            
        <mdf:DdsRecord id="_RSIGNON" runat="server" style="POSITION: relative;"  Alias="RSIGNON" CssClass="DdsRecord" NavigationBarControlID="HomeNavBar" AttnKeys="F3 03" EraseFormats="*ALL" >

            <mdf:DdsBar ID="HomeNavBar" runat="server" CssClass="DdsBar" Width="100%" >
                <mdf:DdsBarSegment ID="DdsBarSegment2" runat="server" Alignment="Left">
                    <mdf:DdsButton ID="DdsButton15" ButtonStyle="Icon" runat="server" CssClass="NavButtonIcon" AidKey="F3" IconID="signout" />
                </mdf:DdsBarSegment>
                <mdf:DdsBarSegment ID="DdsBarSegment3" runat="server" Alignment="Center">
                    <span class="PanelTitle" >Sign On</span>
                </mdf:DdsBarSegment>
                <mdf:DdsBarSegment ID="DdsBarSegment1" runat="server" Alignment="Center">
                </mdf:DdsBarSegment>
            </mdf:DdsBar>
            <br />

            <mdf:DdsPanel ID="Panel" runat="server"  VisibleCondition="!30" >
                <mdf:DdsConstant id="DdsConstant7" runat="server" Text="System:" CssClass="DdsConstant" Width="5em"   />
                <mdf:DdsCharField id="RSignon_System" runat="server" CssClass="DdsCharField" Alias="SYSTEM" Length="126" Usage="Both" Lower="True" Width="10em" />
                <br />
                <mdf:DdsConstant id="DdsConstant21" runat="server" Text="Port:" CssClass="DdsConstant" Width="5em" />
                <mdf:DdsCharField id="RSignon_Port" runat="server" CssClass="DdsDecField" Alias="Port" Length="5" Usage="Both" Width="10em" />
                <br />
            </mdf:DdsPanel>

            <mdf:DdsConstant id="DdsConstant1" runat="server" Text="User:" CssClass="DdsConstant" Width="5em" />
            <mdf:DdsCharField id="RSignon_User" runat="server" CssClass="DdsCharField" Alias="USER" Length="10" Usage="Both" Lower="True" Width="10em"  />
            <br />
            <mdf:DdsConstant id="DdsConstant2" runat="server" Text="Password:" CssClass="DdsConstant" Width="5em" />
            <mdf:DdsCharField id="RSignon_Password" runat="server" CssClass="DdsCharField" Alias="Password" Length="128" Usage="Both" Lower="True" InputStyle="Password" Width="10em" PositionCursor = "*True"/>
            <br />         
            <br /> 
            <div class="SignOnButton">
                <mdf:DdsButton ID="DdsButton1" ButtonStyle="Button" runat="server" AidKey="Enter" Text="Sign On" CssClass="SubmitButton" />
            </div>     
            <br />              
              
            <mdf:DdsCharField id="RSignon_Message" runat="server" CssClass="DdsSflMsgField" Alias="Message" Length="100" Usage="OutputOnly" Lower="True" />

        </mdf:DdsRecord >

        <mdf:DdsRecord id="_RNEWPASS" runat="server" style="POSITION: relative;"  Alias="RNEWPASS" CssClass="DdsRecord" NavigationBarControlID="ResetNavBar" AttnKeys="F3 03" EraseFormats="*ALL" >

            <mdf:DdsBar ID="ResetNavBar" runat="server" CssClass="DdsBar" Width="100%" >
                <mdf:DdsBarSegment ID="DdsBarSegment4" runat="server" Alignment="Left">
                    <mdf:DdsButton ID="DdsButton2" ButtonStyle="Icon" runat="server" CssClass="NavButtonIcon" AidKey="F3" IconID="signout" />
                </mdf:DdsBarSegment>
                <mdf:DdsBarSegment ID="DdsBarSegment5" runat="server" Alignment="Center">
                    <span class="PanelTitle" >Reset Password</span>
                </mdf:DdsBarSegment>
                <mdf:DdsBarSegment ID="DdsBarSegment6" runat="server" Alignment="Center">
                </mdf:DdsBarSegment>
            </mdf:DdsBar>
            <br />

            <mdf:DdsConstant id="DdsConstant5" runat="server"  CssClass="DdsConstant" Text="Password has expired.  Password must be changed to continue." Color="Red : *true" />
            <br />         
            <br /> 
            <mdf:DdsConstant id="DdsConstant6" runat="server" CssClass="DdsConstant" Text="Current Password:" Width="11em" />
            <mdf:DdsCharField id="RNewPass_Password" runat="server" CssClass="DdsCharField" Alias="PASSWORD" Length="128" Lower="True" 
              Usage="Both" Width="10em" InputStyle="Password"  />
            <br />
            <br />
            <mdf:DdsConstant id="DdsConstant3" runat="server" CssClass="DdsConstant" Text="New Password:" Width="11em" />
            <mdf:DdsCharField id="RNewPass_NewPass" runat="server" CssClass="DdsCharField" Alias="NEWPASS" Length="128" Lower="True" 
              Usage="Both" Width="10em" InputStyle="Password"  />
            <br />
            <mdf:DdsConstant id="DdsConstant8" runat="server" CssClass="DdsConstant" Text="Confirm New Password:" Width="11em" />
            <mdf:DdsCharField id="RNewPass_Confirm" runat="server" CssClass="DdsCharField" Alias="CONFIRM" Length="128" Lower="True" 
              Usage="Both" Width="10em" InputStyle="Password" />

            <br />         
            <br /> 
            <div class="SignOnButton">
                <mdf:DdsButton ID="DdsButton3" ButtonStyle="Button" runat="server" AidKey="Enter" Text="Reset Password" CssClass="SubmitButton" />
            </div>     
            <br />              

            <mdf:DdsCharField id="RNewPass_Message" runat="server" CssClass="DdsSflMsgField" Alias="Message" Length="100" Usage="OutputOnly" />
              
        </mdf:DdsRecord >

    </asp:Content>

 <asp:Content ID="Content2" ContentPlaceHolderID="PageScriptPH" runat="server">
 </asp:Content>
