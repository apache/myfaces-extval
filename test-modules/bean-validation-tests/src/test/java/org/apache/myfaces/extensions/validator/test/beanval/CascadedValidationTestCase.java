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
package org.apache.myfaces.extensions.validator.test.beanval;

import junit.framework.Test;
import junit.framework.TestSuite;
import org.apache.myfaces.extensions.validator.test.beanval.model.CascadedValidationTestCase1Bean;
import org.apache.myfaces.extensions.validator.test.beanval.model.CustomTypeForCascadedValidationTestCase1Bean;

import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;
import javax.faces.component.UIComponent;

public class CascadedValidationTestCase extends BaseBeanValPropertyValidationTestCase<CascadedValidationTestCase1Bean>
{
    public CascadedValidationTestCase(String name)
    {
        super(name);
    }

    public static Test suite()
    {
        return new TestSuite(CascadedValidationTestCase.class);
    }

    protected CascadedValidationTestCase1Bean getBeanToTest()
    {
        return new CascadedValidationTestCase1Bean();
    }

    public void testCascadedValidationCorrectValue()
    {
        createValueBindingForComponent(this.inputComponent1, "#{testBean.property}");
        setValueToValidate(this.inputComponent1, "123:456");
        this.inputComponent1.setConverter(createCustomTypeConverter());

        validateComponents();

        assertComponentValid(this.inputComponent1);
        assertNavigationBlocked(false);

        checkMessageCount(0);
    }

    public void testCascadedValidationValueTooShort()
    {
        createValueBindingForComponent(this.inputComponent1, "#{testBean.property}");
        setValueToValidate(this.inputComponent1, "1:4");
        this.inputComponent1.setConverter(createCustomTypeConverter());

        validateComponents();

        assertComponentInvalid(this.inputComponent1);
        assertNavigationBlocked(true);

        checkMessageCount(1);
        checkMessageSeverities(FacesMessage.SEVERITY_ERROR);
    }

    public void testCascadedValidationInvalidValue()
    {
        createValueBindingForComponent(this.inputComponent1, "#{testBean.property}");
        setValueToValidate(this.inputComponent1, "");
        this.inputComponent1.setConverter(createCustomTypeConverter());

        validateComponents();

        assertComponentInvalid(this.inputComponent1);
        assertNavigationBlocked(true);

        checkMessageCount(1);
        checkMessageSeverities(FacesMessage.SEVERITY_ERROR);
    }

    private Converter createCustomTypeConverter()
    {
        return new Converter()
        {
            public Object getAsObject(FacesContext facesContext, UIComponent uiComponent, String s) throws ConverterException
            {
                if(s != null && !s.equals(""))
                {
                    return new CustomTypeForCascadedValidationTestCase1Bean(s.split(":")[0], s.split(":")[1]);
                }
                return null;
            }

            public String getAsString(FacesContext facesContext, UIComponent uiComponent, Object o) throws ConverterException
            {
                return ((CustomTypeForCascadedValidationTestCase1Bean)o).getProperty1() + "" + ((CustomTypeForCascadedValidationTestCase1Bean)o).getProperty2();
            }
        };
    }
}
