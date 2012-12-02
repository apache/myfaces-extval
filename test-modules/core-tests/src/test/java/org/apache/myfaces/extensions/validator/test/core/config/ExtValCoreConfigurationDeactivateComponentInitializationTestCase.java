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

import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.initializer.component.ComponentInitializer;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

/**
 * 
 * since v4
 *
 */
public class ExtValCoreConfigurationDeactivateComponentInitializationTestCase extends ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationDeactivateComponentInitializationTestCase(String name)
    {
        super(name);
    }

    public static class CustomComponentInitializer implements ComponentInitializer
    {

        public void configureComponent(FacesContext facesContext, UIComponent uiComponent, Map<String, Object> metaData)
        {

        }

    }

    @Override
    protected void setUp() throws Exception
    {

        super.setUp();
        // Make sure we have one ComponentInitializer. When deactivated the
        // getComponentInitializers returns an empty list whatever registered.
        ExtValContext.getContext().addComponentInitializer(new CustomComponentInitializer());

    }

    @Override
    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".DEACTIVATE_COMPONENT_INITIALIZATION", "true");

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
                public boolean deactivateComponentInitialization()
                {
                    return true;
                }
            };
        }
        else
        {
            return null;
        }
    }

    public void testDeactivateComponentInitializationDefault()
    {

        assertFalse(ExtValContext.getContext().getComponentInitializers().isEmpty());
    }

    public void testDeactivateComponentInitializationWebXml()
    {
        assertTrue(ExtValContext.getContext().getComponentInitializers().isEmpty());
    }

    public void testDeactivateComponentInitializationCustomConfig()
    {
        assertTrue(ExtValContext.getContext().getComponentInitializers().isEmpty());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(ExtValCoreConfigurationDeactivateComponentInitializationTestCase.class);
    }

}
