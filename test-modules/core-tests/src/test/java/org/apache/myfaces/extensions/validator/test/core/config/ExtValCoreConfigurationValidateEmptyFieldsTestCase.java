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
package org.apache.myfaces.extensions.validator.test.core.config;

import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import junit.framework.Test;

import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.initializer.component.AbstractHtmlCoreComponentsComponentInitializer;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

/**
 * 
 * @author Rudy De Busscher
 * since v4
 *
 */
public class ExtValCoreConfigurationValidateEmptyFieldsTestCase extends ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationValidateEmptyFieldsTestCase(String name)
    {
        super(name);
    }

    public static class CustomComponentInitializer extends AbstractHtmlCoreComponentsComponentInitializer
    {

        @Override
        protected void configureRequiredAttribute(FacesContext facesContext, UIComponent uiComponent,
                Map<String, Object> metaData)
        {

        }

        @Override
        public boolean validateEmptyFields()
        {
            return super.validateEmptyFields();
        }

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter("javax.faces.VALIDATE_EMPTY_FIELDS", "false");
        }
    }

    @Override
    protected ExtValCoreConfiguration getCustomExtValCoreConfiguration()
    {
        if (needCustomConfig())
        {

            return new DefaultExtValCoreConfiguration()
            {
                @Override
                public boolean validateEmptyFields()
                {
                    return false;
                }
            };
        }
        else
        {
            return null;
        }
    }

    public void testValidateEmptyFieldsDefault()
    {
        CustomComponentInitializer initializer = new CustomComponentInitializer();
        assertTrue(initializer.validateEmptyFields());
    }

    public void testValidateEmptyFieldsWebXml()
    {
        CustomComponentInitializer initializer = new CustomComponentInitializer();
        assertFalse(initializer.validateEmptyFields());
    }

    public void testValidateEmptyFieldsCustomConfig()
    {
        CustomComponentInitializer initializer = new CustomComponentInitializer();
        assertFalse(initializer.validateEmptyFields());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(ExtValCoreConfigurationValidateEmptyFieldsTestCase.class);
    }

}
