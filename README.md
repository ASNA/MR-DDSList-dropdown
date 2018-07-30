### Using Mobile RPG's DDSList as a dropdown

The DDSList is a frequently-used control for Mobile RPG.  It has four distinct "personalties":

1. A navigation list.
2. A radiobutton list (which provides a radiobutton list that accepts multiple choices).
3. A checkbox list (which provides a checkbox list that accepts one choice).
4. A dropdown list. 

This article discussing how to use the DDSList control to provide a dropdown element for a Mobile RPG record format. When rendered this way, the generated HTML is a `select` tag with `option` tags for each item in the dropdown list. This article's example code is shown below in Figure 1a. When the `OK` button is clicked the selected value of the dropdown is shown in the textbox just above the `OK` button.

![](http://asna.com/filebin/marketing/article-figures/mobile-rp-dropdown-example.png)
<small>Figure 1a. The DDSList rendered as a dropdown (select tag)</small>

In every case, the rendered element is populated with columns in an RPG subfile.  The properties of the DDSList specify the field names and other subfile attributes. The list in Figure 1b below shows the DDSList properties, and their values, used in this example to render the dropdown.

| Property | Value |Description
|:------------------ |:-------- |:--
| ListType | Dropdown |This field indicates the type of list rendered by the DDSList control. It can be Navigation, Radiobutton, Checkbox, or Dropdown.
| ClearIndicator | 99 |Indicator to clear subfile
| SelectedField | DDSel |This is a hidden field in the subfile row. This field is required but isn't used when the DDSList is used as a dropdown.
| SelectedItemField | DDCurrVal |When the DDSList is used as a dropdown, this field is a getter/setter for the dropdown's selected value. This field is always the same length as the `ValueFieldLength` and is a hidden field on the Mobile RPG record format (it is _not_ a column in the DDSList's subfile).
| SubfileName | sbfDD |The name of the subfile associated with this DDSList dropdown.
| SubfileControlName | sbfDDCtl |The name of the subfile controller associated with this DDSList dropdown's subfile.
| TextField | DDText | The name of a field that is a column in the DDSList's subfile that provides the displayed text for the dropdown, 
| TextFieldLength | 24 |The length of the field specified in the TextField property.
| ValueField | DDValue |The name of a field that is a column in the DDSList's subfile that provides the value associated with the TextField property. 
| ValueFieldLength | 2 | The length of the field specified in the ValueField property.

<small>Figure 1b. DDSList properties used in this example to render the dropdown.</small>

The code below in Figure 1c shows how this DDSList looks in the ASPX markup:

	 <mdf:DdsList ID="DdsList1" runat="server" 
	    CssClass="DdsList" CssClassChevron="DdsListChevron" 
	    CssClassDetail="DdsListDetail" CssClassIcon="DdsListIcon" 
	    CssClassImage="DdsListImage" CssClassList="DdsListList" 
	    CssClassRow="DdsListRow" 
	    ListType="Dropdown" 
	    ClearIndicator="99" 
	    SelectedField="DDSel" 
	    SelectedItemField="DDCurrVal" 
	    SubfileName="sbfDD" 
	    SubfileControlName="sbfDDCtl" 
	    TextField="DDText" 
	    TextFieldLength="24" 
	    ValueField="DDValue" 
	    ValueFieldLength="2" 
	/>
<small>Figure 1c. The DDSList as defined in the ASPX markup.</small>

To fully understand the relationship between a Mobile RPG control and its underlying RPG program, it often helps to save the corresponding DDS source while exporting the Mobile RPG display file to the IBM i. Do this by using the options in the red box below in Figure 1d. 

![](http://asna.com/filebin/marketing/article-figures/ddslist-as-dropdown-save.png)

<small>Figure 1d. How to save the DDS source when exporting a Mobile RPG display file to the IBM i</small>

The DDSList dropdown's exported DDS source is shown below in Fibgure 1e. This source lets you clearly see that there are three columns in the subfile (two of which are hidden).  

The DDRList's `SelectField` property produces the DDSEL field. For most subfile-driven Mobile RPG controls, this field is set to "1" when a row is selected and READC is used to read the selected rows. Because Mobile RPG's dropdown only ever has a single selected value, the DDSList's SelectedItemField specifies an RPG file that is a getter and setter for the dropdown's value. As you can see in the DDS source in the figure below, the property this field products (DDCURRVAL) is hidden. The DDSList's dropdown logic automatically puts the dropdown's selected value in this field. If you set this field in your RPG to a value in the list before showing the page, the list will be positioned at that value's list position.
  
    A            DDCURRVAL      2A  H
    A            STATE         10A  B  1  6
    A          R SBFDD                     SFL
    A            DDSEL          1A  H
    A            DDTEXT        24A  O  1  5
    A            DDVALUE        2A  H
    A          R SBFDDCTL                  SFLCTL(SBFDD)
    A                                      SFLSIZ(2)
    A                                      SFLPAG(1)
    A N99                                  SFLDSP
    A N99                                  SFLDSPCTL
    A  99                                  SFLCLR
    A                                      OVERLAY
<small>Figure 1e. How to save the DDS source when exporting a Mobile RPG display file to the IBM i</small>
