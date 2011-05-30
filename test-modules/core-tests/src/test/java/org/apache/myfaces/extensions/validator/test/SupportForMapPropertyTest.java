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
package org.apache.myfaces.extensions.validator.test;

import org.apache.myfaces.extensions.validator.test.base.AbstractExValTestCase;
import org.junit.Test;

import java.util.HashMap;
import java.util.Map;

import javax.faces.component.UIViewRoot;
import javax.faces.component.html.HtmlForm;
import javax.faces.component.html.HtmlInputText;


//just a test for the mock impl.
public class SupportForMapPropertyTest extends AbstractExValTestCase
{
    HtmlInputText inputComponent1 = null;

    UIViewRoot rootComponent = null;

    @Override
    protected void invokeStartupListeners()
    {
        //do nothing
    }

    @Override
    protected void setUpTestCase()
    {
        super.setUpTestCase();
        TestBean bean = new TestBean();
        createValueBinding(null, "value", "#{testBean}");
        facesContext.getExternalContext().getRequestMap().put("testBean", bean);

        rootComponent = new UIViewRoot();
        HtmlForm form = new HtmlForm();
        form.setId("form");
        rootComponent.getChildren().add(form);
        inputComponent1 = new HtmlInputText();
        form.getChildren().add(inputComponent1);
        inputComponent1.setId("input1");

    }

    @Test
    public void testMapPropertyFromScreen()
    {
        createValueBinding(inputComponent1, "value", "#{testBean.mapProperty['Key']}");

        //decode
        inputComponent1.setSubmittedValue("value1");

        //validate
        inputComponent1.validate(facesContext);
    }

    public class TestBean
    {
        private Map<String, String> mapProperty;

        public TestBean()
        {
            mapProperty = new HashMap<String, String>();
            mapProperty.put("model", "ModelValue");
        }

        public void setMapProperty(Map<String, String> mapProperty)
        {
            this.mapProperty = mapProperty;
        }

        public Map<String, String> getMapProperty()
        {
            return mapProperty;
        }
    }
}
