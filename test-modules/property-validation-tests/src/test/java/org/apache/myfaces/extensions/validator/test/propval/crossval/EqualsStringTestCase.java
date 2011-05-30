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
package org.apache.myfaces.extensions.validator.test.propval.crossval;


import org.apache.myfaces.extensions.validator.test.propval.AbstractPropertyValidationTestCase;
import org.junit.Test;

import javax.faces.component.UIViewRoot;
import javax.faces.component.html.HtmlForm;
import javax.faces.component.html.HtmlInputText;

public class EqualsStringTestCase extends AbstractPropertyValidationTestCase
{

    HtmlInputText inputComponent1 = null;
    HtmlInputText inputComponent2 = null;

    UIViewRoot rootComponent = null;

    public EqualsStringTestCase()
    {
        inputComponent1 = null;
        inputComponent2 = null;
        rootComponent = null;
    }


    @Override
    protected void setUpTestCase()
    {
        super.setUpTestCase();
        EqualsStringTestBean bean = new EqualsStringTestBean();
        createValueBinding(null, "value", "#{testBean}");
        facesContext.getExternalContext().getRequestMap().put("testBean", bean);

        rootComponent = new UIViewRoot();
        HtmlForm form = new HtmlForm();
        form.setId("form");
        rootComponent.getChildren().add(form);
        inputComponent1 = new HtmlInputText();
        form.getChildren().add(inputComponent1);
        inputComponent1.setId("input1");
        inputComponent2 = new HtmlInputText();
        form.getChildren().add(inputComponent2);
        inputComponent2.setId("input2");
    }

    @Test
    public void testEqualsCaseSensitiveSame() throws Exception
    {
        createValueBinding(inputComponent1, "value", "#{testBean.property1}");
        createValueBinding(inputComponent2, "value", "#{testBean.property2}");

        //decode
        inputComponent1.setSubmittedValue("1d3");
        inputComponent2.setSubmittedValue("1d3");

        //validate
        inputComponent1.validate(facesContext);
        inputComponent2.validate(facesContext);

        processCrossValidation();
        checkMessageCount(0);
    }

    @Test
    public void testEqualsCaseSensitiveDifferent() throws Exception
    {
        createValueBinding(inputComponent1, "value", "#{testBean.property1}");
        createValueBinding(inputComponent2, "value", "#{testBean.property2}");

        //decode
        inputComponent1.setSubmittedValue("1d3");
        inputComponent2.setSubmittedValue("1D3");

        //validate
        inputComponent1.validate(facesContext);
        inputComponent2.validate(facesContext);

        processCrossValidation();
        checkMessageCount(2); // For each field a message
    }

    @Test
    public void testEqualsCaseInsensitiveSame() throws Exception
    {
        createValueBinding(inputComponent1, "value", "#{testBean.property1}");
        createValueBinding(inputComponent2, "value", "#{testBean.property3}");

        //decode
        inputComponent1.setSubmittedValue("1d3");
        inputComponent2.setSubmittedValue("1d3");

        //validate
        inputComponent1.validate(facesContext);
        inputComponent2.validate(facesContext);

        processCrossValidation();
        checkMessageCount(0);
    }

    @Test
    public void testEqualsCaseInsensitiveDifferent() throws Exception
    {
        createValueBinding(inputComponent1, "value", "#{testBean.property1}");
        createValueBinding(inputComponent2, "value", "#{testBean.property3}");

        //decode
        inputComponent1.setSubmittedValue("1d3");
        inputComponent2.setSubmittedValue("1D3");

        //validate
        inputComponent1.validate(facesContext);
        inputComponent2.validate(facesContext);

        processCrossValidation();
        checkMessageCount(0); // Due to extra parameter attribute, no longer an error.
    }
}
