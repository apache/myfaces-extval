/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.apache.myfaces.extensions.validator.core.client;

import org.apache.myfaces.extensions.validator.util.ArrayUtils;


/**
 * copied in order to provide it to custom client-side validators
 * <p/>
 * Constant declarations for HTML rendering.
 *
 * @author Manfred Geiler
 * @author Anton Koinov
 * @version $Revision: 472618 $ $Date: 2006-11-08 20:06:54 +0000 (Wed, 08 Nov 2006) $
 */
public interface HTML
{

    // Common attributes
    String ALIGN_ATTR = "align";
    String DATAFLD_ATTR = "datafld";
    String DATASRC_ATTR = "datasrc";
    String DATAFORMATAS_ATTR = "dataformatas";
    String BORDER_ATTR = "border";
    String WIDTH_ATTR = "width";
    String READONLY_ATTR = "readonly";
    String FILE_ATTR = "file";
    String ACCEPT_ATTR = "accept";

    // Common event handler attributes
    String ONCLICK_ATTR = "onclick";
    String ONDBLCLICK_ATTR = "ondblclick";
    String ONMOUSEDOWN_ATTR = "onmousedown";
    String ONMOUSEUP_ATTR = "onmouseup";
    String ONMOUSEOVER_ATTR = "onmouseover";
    String ONMOUSEMOVE_ATTR = "onmousemove";
    String ONMOUSEOUT_ATTR = "onmouseout";
    String ONKEYPRESS_ATTR = "onkeypress";
    String ONKEYDOWN_ATTR = "onkeydown";
    String ONKEYUP_ATTR = "onkeyup";
    String[] EVENT_HANDLER_ATTRIBUTES_WITHOUT_ONCLICK =
        {
            ONDBLCLICK_ATTR,
            ONMOUSEDOWN_ATTR,
            ONMOUSEUP_ATTR,
            ONMOUSEOVER_ATTR,
            ONMOUSEMOVE_ATTR,
            ONMOUSEOUT_ATTR,
            ONKEYPRESS_ATTR,
            ONKEYDOWN_ATTR,
            ONKEYUP_ATTR
        };
    String[] EVENT_HANDLER_ATTRIBUTES =
        (String[]) ArrayUtils.concat(
            EVENT_HANDLER_ATTRIBUTES_WITHOUT_ONCLICK,
            new String[]{ONCLICK_ATTR});

    // Input field event handler attributes
    String ONFOCUS_ATTR = "onfocus";
    String ONBLUR_ATTR = "onblur";
    String ONSELECT_ATTR = "onselect";
    String ONCHANGE_ATTR = "onchange";
    String[] COMMON_FIELD_EVENT_ATTRIBUTES =
        {
            ONFOCUS_ATTR,
            ONBLUR_ATTR,
            ONSELECT_ATTR,
            ONCHANGE_ATTR
        };

    String[] COMMON_FIELD_EVENT_ATTRIBUTES_WITHOUT_ONFOCUS =
        {
            ONBLUR_ATTR,
            ONSELECT_ATTR,
            ONCHANGE_ATTR
        };

    // universal attributes
    String DIR_ATTR = "dir";
    String LANG_ATTR = "lang";
    String STYLE_ATTR = "style";
    String TITLE_ATTR = "title";
    String STYLE_CLASS_ATTR = "styleClass"; //"class" cannot be used as property name

    String[] UNIVERSAL_ATTRIBUTES_WITHOUT_STYLE =
        {
            DIR_ATTR,
            LANG_ATTR,
            TITLE_ATTR,

            //NOTE: if changed, please verify universal attributes in HtmlMessageRenderer !
        };
    String[] UNIVERSAL_ATTRIBUTES =
        (String[]) ArrayUtils.concat(
            UNIVERSAL_ATTRIBUTES_WITHOUT_STYLE,
            new String[]{STYLE_ATTR, STYLE_CLASS_ATTR});

    //universal, but not the same property-name -
    //styleClass attribute is rendered as such
    String CLASS_ATTR = "class";

    // common form field attributes
    String ACCESSKEY_ATTR = "accesskey";
    String TABINDEX_ATTR = "tabindex";
    String DISABLED_ATTR = "disabled";
    String[] COMMON_FIELD_ATTRIBUTES_WITHOUT_DISABLED =
        {
            ACCESSKEY_ATTR,
            TABINDEX_ATTR
        };
    String[] COMMON_FIELD_ATTRIBUTES =
        (String[]) ArrayUtils.concat(
            COMMON_FIELD_ATTRIBUTES_WITHOUT_DISABLED,
            new String[]{DISABLED_ATTR});

    // Common Attributes
    String[] COMMON_PASSTROUGH_ATTRIBUTES =
        (String[]) ArrayUtils.concat(
            EVENT_HANDLER_ATTRIBUTES,
            UNIVERSAL_ATTRIBUTES);
    String[] COMMON_PASSTROUGH_ATTRIBUTES_WITHOUT_STYLE =
        (String[]) ArrayUtils.concat(
            EVENT_HANDLER_ATTRIBUTES,
            UNIVERSAL_ATTRIBUTES_WITHOUT_STYLE);
    String[] COMMON_PASSTROUGH_ATTRIBUTES_WITHOUT_ONCLICK =
        (String[]) ArrayUtils.concat(
            EVENT_HANDLER_ATTRIBUTES_WITHOUT_ONCLICK,
            UNIVERSAL_ATTRIBUTES);
    String[] COMMON_PASSTROUGH_ATTRIBUTES_WITHOUT_ONCLICK_WITHOUT_STYLE =
        (String[]) ArrayUtils.concat(
            EVENT_HANDLER_ATTRIBUTES_WITHOUT_ONCLICK,
            UNIVERSAL_ATTRIBUTES_WITHOUT_STYLE);
    String[] COMMON_FIELD_PASSTROUGH_ATTRIBUTES_WITHOUT_DISABLED =
        (String[]) ArrayUtils.concat(
            COMMON_PASSTROUGH_ATTRIBUTES,
            COMMON_FIELD_ATTRIBUTES_WITHOUT_DISABLED,
            COMMON_FIELD_EVENT_ATTRIBUTES);
    String[] COMMON_FIELD_PASSTROUGH_ATTRIBUTES_WITHOUT_DISABLED_AND_ONFOCUS =
        (String[]) ArrayUtils.concat(
            COMMON_PASSTROUGH_ATTRIBUTES,
            COMMON_FIELD_ATTRIBUTES_WITHOUT_DISABLED,
            COMMON_FIELD_EVENT_ATTRIBUTES_WITHOUT_ONFOCUS);
    String[] COMMON_FIELD_PASSTROUGH_ATTRIBUTES_WITHOUT_DISABLED_AND_ONFOCUS_AND_ONCLICK =
        (String[]) ArrayUtils.concat(
            COMMON_PASSTROUGH_ATTRIBUTES_WITHOUT_ONCLICK,
            COMMON_FIELD_ATTRIBUTES_WITHOUT_DISABLED,
            COMMON_FIELD_EVENT_ATTRIBUTES_WITHOUT_ONFOCUS);
    String[] COMMON_FIELD_PASSTROUGH_ATTRIBUTES_WITHOUT_DISABLED_AND_ONCLICK =
        (String[]) ArrayUtils.concat(
            COMMON_PASSTROUGH_ATTRIBUTES_WITHOUT_ONCLICK,
            COMMON_FIELD_ATTRIBUTES_WITHOUT_DISABLED,
            COMMON_FIELD_EVENT_ATTRIBUTES);

    // <a>
    String TARGET_ATTR = "target";  //used by <a> and <form>
    String CHARSET_ATTR = "charset";
    String COORDS_ATTR = "coords";
    String HREF_ATTR = "href";
    String HREFLANG_ATTR = "hreflang";
    String REL_ATTR = "rel";
    String REV_ATTR = "rev";
    String SHAPE_ATTR = "shape";
    String TYPE_ATTR = "type";
    String[] ANCHOR_ATTRIBUTES =
        {
            ACCESSKEY_ATTR,
            CHARSET_ATTR,
            COORDS_ATTR,
            HREFLANG_ATTR,
            REL_ATTR,
            REV_ATTR,
            SHAPE_ATTR,
            TABINDEX_ATTR,
            TARGET_ATTR,
            TYPE_ATTR
        };
    String[] ANCHOR_PASSTHROUGH_ATTRIBUTES =
        (String[]) ArrayUtils.concat(
            ANCHOR_ATTRIBUTES,
            COMMON_PASSTROUGH_ATTRIBUTES);
    String[] ANCHOR_PASSTHROUGH_ATTRIBUTES_WITHOUT_STYLE =
        (String[]) ArrayUtils.concat(
            ANCHOR_ATTRIBUTES,
            COMMON_PASSTROUGH_ATTRIBUTES_WITHOUT_STYLE);
    String[] ANCHOR_PASSTHROUGH_ATTRIBUTES_WITHOUT_ONCLICK_WITHOUT_STYLE =
        (String[]) ArrayUtils.concat(
            ANCHOR_ATTRIBUTES,
            COMMON_PASSTROUGH_ATTRIBUTES_WITHOUT_ONCLICK_WITHOUT_STYLE);

    // <form>
    String ACCEPT_CHARSET_ATTR = "accept-charset";
    String ENCTYPE_ATTR = "enctype";
    String ONRESET_ATTR = "onreset";
    String ONSUMBIT_ATTR = "onsubmit";
    String[] FORM_ATTRIBUTES =
        {
            ACCEPT_ATTR,
            ACCEPT_CHARSET_ATTR,
            ENCTYPE_ATTR,
            ONRESET_ATTR,
            ONSUMBIT_ATTR,
            TARGET_ATTR,
        };
    String[] FORM_PASSTHROUGH_ATTRIBUTES =
        (String[]) ArrayUtils.concat(
            FORM_ATTRIBUTES,
            COMMON_PASSTROUGH_ATTRIBUTES);

    // <img>
    String SRC_ATTR = "src";
    String ALT_ATTR = "alt";
    String HEIGHT_ATTR = "height";
    String HSPACE_ATTR = "hspace";
    String ISMAP_ATTR = "ismap";
    String LONGDESC_ATTR = "longdesc";
    String USEMAP_ATTR = "usemap";
    String VSPACE_ATTR = "vspace";

    String[] IMG_ATTRIBUTES =
        {
            ALIGN_ATTR,
            ALT_ATTR,
            BORDER_ATTR,
            HEIGHT_ATTR,
            HSPACE_ATTR,
            ISMAP_ATTR,
            LONGDESC_ATTR,
            USEMAP_ATTR,
            VSPACE_ATTR,
            WIDTH_ATTR
        };
    String[] IMG_PASSTHROUGH_ATTRIBUTES =
        (String[]) ArrayUtils.concat(
            IMG_ATTRIBUTES,
            COMMON_PASSTROUGH_ATTRIBUTES);

    // <input>
    String SIZE_ATTR = "size";
    String AUTOCOMPLETE_ATTR = "autocomplete";
    String CHECKED_ATTR = "checked";
    String MAXLENGTH_ATTR = "maxlength";

    String[] INPUT_ATTRIBUTES = {
        ALIGN_ATTR,
        ALT_ATTR,
        CHECKED_ATTR,
        DATAFLD_ATTR,
        DATASRC_ATTR,
        DATAFORMATAS_ATTR,
        MAXLENGTH_ATTR,
        READONLY_ATTR,
        SIZE_ATTR,
        AUTOCOMPLETE_ATTR
    };
    String[] INPUT_PASSTHROUGH_ATTRIBUTES_WITHOUT_DISABLED =
        (String[]) ArrayUtils.concat(
            INPUT_ATTRIBUTES,
            COMMON_FIELD_PASSTROUGH_ATTRIBUTES_WITHOUT_DISABLED);

    String[] INPUT_PASSTHROUGH_ATTRIBUTES_WITHOUT_DISABLED_AND_ONFOCUS_AND_ONCLICK =
        (String[]) ArrayUtils.concat(
            INPUT_ATTRIBUTES,
            COMMON_FIELD_PASSTROUGH_ATTRIBUTES_WITHOUT_DISABLED_AND_ONFOCUS_AND_ONCLICK);

    //values for input-type attribute
    String INPUT_TYPE_SUBMIT = "submit";
    String INPUT_TYPE_IMAGE = "image";
    String INPUT_TYPE_HIDDEN = "hidden";
    String INPUT_TYPE_CHECKBOX = "checkbox";
    String INPUT_TYPE_PASSWORD = "password";
    String INPUT_TYPE_TEXT = "text";
    String INPUT_TYPE_RADIO = "radio";
    String INPUT_TYPE_BUTTON = "button";

    // <button>
    String[] BUTTON_ATTRIBUTES =
        {
            ALIGN_ATTR,
            ALT_ATTR,
            DATAFLD_ATTR,
            DATASRC_ATTR,
            DATAFORMATAS_ATTR,
        };
    String[] BUTTON_PASSTHROUGH_ATTRIBUTES_WITHOUT_DISABLED =
        (String[]) ArrayUtils.concat(
            BUTTON_ATTRIBUTES,
            COMMON_FIELD_PASSTROUGH_ATTRIBUTES_WITHOUT_DISABLED);
    String[] BUTTON_PASSTHROUGH_ATTRIBUTES_WITHOUT_DISABLED_AND_ONCLICK =
        (String[]) ArrayUtils.concat(
            BUTTON_ATTRIBUTES,
            COMMON_FIELD_PASSTROUGH_ATTRIBUTES_WITHOUT_DISABLED_AND_ONCLICK);

    // <iframe>
    String FRAMEBORDER_ATTR = "frameborder";
    String SCROLLING_ATTR = "scrolling";

    // <label>
    String FOR_ATTR = "for";
    String[] LABEL_ATTRIBUTES =
        {
            ACCESSKEY_ATTR,
            ONBLUR_ATTR,
            ONFOCUS_ATTR
            //FOR_ATTR is no pass through !
        };
    String[] LABEL_PASSTHROUGH_ATTRIBUTES =
        (String[]) ArrayUtils.concat(
            LABEL_ATTRIBUTES,
            COMMON_PASSTROUGH_ATTRIBUTES);

    // <select>
    String MULTIPLE_ATTR = "multiple";

    String[] SELECT_ATTRIBUTES =
        {
            DATAFLD_ATTR,
            DATASRC_ATTR,
            DATAFORMATAS_ATTR,
        };
    String[] SELECT_PASSTHROUGH_ATTRIBUTES_WITHOUT_DISABLED =
        (String[]) ArrayUtils.concat(
            SELECT_ATTRIBUTES,
            COMMON_FIELD_PASSTROUGH_ATTRIBUTES_WITHOUT_DISABLED);

    // <table>
    String BGCOLOR_ATTR = "bgcolor";
    String CELLPADDING_ATTR = "cellpadding";
    String CELLSPACING_ATTR = "cellspacing";
    String FRAME_ATTR = "frame";
    String RULES_ATTR = "rules";
    String SUMMARY_ATTR = "summary";
    String[] TABLE_ATTRIBUTES = {
        ALIGN_ATTR,
        BGCOLOR_ATTR,
        BORDER_ATTR,
        CELLPADDING_ATTR,
        CELLSPACING_ATTR,
        DATAFLD_ATTR,
        DATASRC_ATTR,
        DATAFORMATAS_ATTR,
        FRAME_ATTR,
        RULES_ATTR,
        SUMMARY_ATTR,
        WIDTH_ATTR
    };
    String[] TABLE_PASSTHROUGH_ATTRIBUTES =
        (String[]) ArrayUtils.concat(
            TABLE_ATTRIBUTES,
            COMMON_PASSTROUGH_ATTRIBUTES);

    // <textarea>
    String COLS_ATTR = "cols";
    String ROWS_ATTR = "rows";
    String WRAP_ATTR = "wrap";
    String[] TEXTAREA_ATTRIBUTES =
        {
            COLS_ATTR,
            DATAFLD_ATTR,
            DATASRC_ATTR,
            DATAFORMATAS_ATTR,
            READONLY_ATTR,
            ROWS_ATTR,
            WRAP_ATTR
        };
    String[] TEXTAREA_PASSTHROUGH_ATTRIBUTES_WITHOUT_DISABLED =
        (String[]) ArrayUtils.concat(
            TEXTAREA_ATTRIBUTES,
            COMMON_FIELD_PASSTROUGH_ATTRIBUTES_WITHOUT_DISABLED);

    // <input type=file>
    String[] INPUT_FILE_UPLOAD_ATTRIBUTES =
        {
            ACCEPT_ATTR
        };
    String[] INPUT_FILE_PASSTHROUGH_ATTRIBUTES_WITHOUT_DISABLED =
        (String[]) ArrayUtils.concat(
            INPUT_FILE_UPLOAD_ATTRIBUTES,
            INPUT_PASSTHROUGH_ATTRIBUTES_WITHOUT_DISABLED);

    /*
String[] MESSAGE_PASSTHROUGH_ATTRIBUTES =
(String[]) ArrayUtils.concat(
    new String[] {DIR_ATTR, LANG_ATTR, TITLE_ATTR, STYLE_ATTR, STYLE_CLASS_ATTR},
    EVENT_HANDLER_ATTRIBUTES);
    */

    String[] MESSAGE_PASSTHROUGH_ATTRIBUTES_WITHOUT_TITLE_STYLE_AND_STYLE_CLASS =
        (String[]) ArrayUtils.concat(
            new String[]{DIR_ATTR, LANG_ATTR},
            EVENT_HANDLER_ATTRIBUTES);


    // selectOne/Many table
    String[] SELECT_TABLE_PASSTHROUGH_ATTRIBUTES =
        new String[]{STYLE_ATTR, STYLE_CLASS_ATTR, BORDER_ATTR};

    String COMPACT_ATTR = "compact";
    String[] UL_ATTRIBUTES = {
        COMPACT_ATTR,
        TYPE_ATTR
    };
    String[] UL_PASSTHROUGH_ATTRIBUTES =
        (String[]) ArrayUtils.concat(
            UL_ATTRIBUTES,
            COMMON_PASSTROUGH_ATTRIBUTES);


    //HTML attributes needed for renderding only
    String ID_ATTR = "id";
    String NAME_ATTR = "name";
    String VALUE_ATTR = "value";
    String METHOD_ATTR = "method";
    String ACTION_ATTR = "action";
    String COLSPAN_ATTR = "colspan";
    String SCOPE_ATTR = "scope";
    String LABEL_ATTR = "label";
    String SELECTED_ATTR = "selected";

    //HTML attributes values
    String SCOPE_COLGROUP_VALUE = "colgroup";

    //HTML element constants
    String SPAN_ELEM = "span";
    String DIV_ELEM = "div";
    String INPUT_ELEM = "input";
    String BUTTON_ELEM = "button";
    String SELECT_ELEM = "select";
    String OPTION_ELEM = "option";
    String OPTGROUP_ELEM = "optgroup";
    String TEXTAREA_ELEM = "textarea";
    String FORM_ELEM = "form";
    String ANCHOR_ELEM = "a";
    String H1_ELEM = "h1";
    String H2_ELEM = "h2";
    String H3_ELEM = "h3";
    String H4_ELEM = "h4";
    String H5_ELEM = "h5";
    String H6_ELEM = "h6";
    String IFRAME_ELEM = "iframe";
    String IMG_ELEM = "img";
    String LABEL_ELEM = "label";
    String TABLE_ELEM = "table";
    String TR_ELEM = "tr";
    String TH_ELEM = "th";
    String TD_ELEM = "td";
    String TBODY_ELEM = "tbody";
    String TFOOT_ELEM = "tfoot";
    String THEAD_ELEM = "thead";
    String STYLE_ELEM = "style";
    String SCRIPT_ELEM = "script";
    String SCRIPT_TYPE_ATTR = "type";
    String SCRIPT_TYPE_TEXT_JAVASCRIPT = "text/javascript";
    String STYLE_TYPE_TEXT_CSS = "text/css";
    String SCRIPT_LANGUAGE_ATTR = "language";
    String SCRIPT_LANGUAGE_JAVASCRIPT = "JavaScript";
    String SCRIPT_ELEM_DEFER_ATTR = "defer";
    String LINK_ELEM = "link";
    String STYLESHEET_VALUE = "stylesheet";
    String UL_ELEM = "ul";
    String OL_ELEM = "ol";
    String LI_ELEM = "li";


    //HTML simple element constants
    String BR_ELEM = "br";


    //HTML entities
    String NBSP_ENTITY = "&#160;";

    String HREF_PATH_SEPARATOR = "/";
    String HREF_PATH_FROM_PARAM_SEPARATOR = "?";
    String HREF_PARAM_SEPARATOR = "&";
    String HREF_PARAM_NAME_FROM_VALUE_SEPARATOR = "=";

}