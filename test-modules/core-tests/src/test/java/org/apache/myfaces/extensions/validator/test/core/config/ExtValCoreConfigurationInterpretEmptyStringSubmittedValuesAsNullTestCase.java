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
import org.apache.myfaces.extensions.validator.core.interceptor.AbstractValidationInterceptor;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

/**
 * 
 * since v4
 *
 */
public class ExtValCoreConfigurationInterpretEmptyStringSubmittedValuesAsNullTestCase extends
        ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationInterpretEmptyStringSubmittedValuesAsNullTestCase(String name)
    {
        super(name);
    }

    public static class TestValidationInterceptor extends AbstractValidationInterceptor
    {

        @Override
        protected MetaDataExtractor getComponentMetaDataExtractor(Map<String, Object> properties)
        {
            return null;
        }

        @Override
        protected void initComponent(FacesContext facesContext, UIComponent uiComponent)
        {

        }

        @Override
        protected void processValidation(FacesContext facesContext, UIComponent uiComponent, Object convertedObject)
        {

        }

        @Override
        public boolean interpretEmptyStringValuesAsNull()
        {
            return super.interpretEmptyStringValuesAsNull();
        }

    }

    @Override
    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter("javax.faces.INTERPRET_EMPTY_STRING_SUBMITTED_VALUES_AS_NULL", "false");
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
                public boolean interpretEmptyStringSubmittedValuesAsNull()
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

    public void testInterpretEmptyStringSubmittedValuesAsNullDefault()
    {
        TestValidationInterceptor interceptor = new TestValidationInterceptor();
        assertTrue(interceptor.interpretEmptyStringValuesAsNull());
    }

    public void testInterpretEmptyStringSubmittedValuesAsNullWebXml()
    {
        TestValidationInterceptor interceptor = new TestValidationInterceptor();
        assertFalse(interceptor.interpretEmptyStringValuesAsNull());
    }

    public void testInterpretEmptyStringSubmittedValuesAsNullCustomConfig()
    {
        TestValidationInterceptor interceptor = new TestValidationInterceptor();
        assertFalse(interceptor.interpretEmptyStringValuesAsNull());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(ExtValCoreConfigurationInterpretEmptyStringSubmittedValuesAsNullTestCase.class);
    }

}
