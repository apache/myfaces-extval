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

import javax.faces.component.UIComponent;
import javax.faces.component.UIInput;
import javax.faces.component.html.HtmlInputText;

import junit.framework.Test;

import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValModuleConfiguration;
import org.apache.myfaces.extensions.validator.core.el.DefaultELHelper;
import org.apache.myfaces.extensions.validator.core.el.ELHelper;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;
import org.apache.myfaces.extensions.validator.test.core.config.support.ConstraintSourceAwareBean;

/**
 * 
 * @author Rudy De Busscher
 * since v4
 *
 */
public class ExtValCoreConfigurationDeactivateElResolverTestCase extends ExtValCoreConfigurationTestCase
{
    private UIInput uiComponent;

    public ExtValCoreConfigurationDeactivateElResolverTestCase(String name)
    {
        super(name);
    }

    public static class CustomDefaultELHelper extends DefaultELHelper
    {

        @Override
        protected PropertyDetails getPropertyDetailsViaReflectionFallback(UIComponent uiComponent)
        {
            return null;
        }

    }

    @Override
    protected void setUp() throws Exception
    {
        super.setUp();
        uiComponent = new HtmlInputText();
        ConstraintSourceAwareBean bean = new ConstraintSourceAwareBean();

        facesContext.getExternalContext().getRequestMap().put("testBean", bean);

        createValueBinding(uiComponent, "value", "#{testBean.property1}");
    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".DEACTIVATE_EL_RESOLVER", "true");
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
                public boolean deactivateElResolver()
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

    @Override
    protected ExtValModuleConfiguration[] getCustomConfigObjects()
    {
        if (needCustomConfig())
        {
            // We need to set it here already, because otherwise the setup triggers the creation
            // of the DefaultELHelper that stores the parameter in a static variable.
            // Setting the parameter to another value through getCustomExtValCoreConfiguration is then pointless.
            return new ExtValModuleConfiguration[] { new DefaultExtValCoreConfiguration()
            {
                @Override
                public boolean deactivateElResolver()
                {
                    return true;
                }

            } };
        }
        else
        {
            return null;
        }
    }

    public void testDeactivateElResolverDefault()
    {
        ELHelper elHelper = new CustomDefaultELHelper();
        assertNotNull(elHelper.getPropertyDetailsOfValueBinding(uiComponent));

    }

    public void testDeactivateElResolverWebXml()
    {
        ELHelper elHelper = new CustomDefaultELHelper();
        // When deactivated, the getPropertyDetailsViaReflectionFallback method
        // is called which returns null in our custom version
        assertNull(elHelper.getPropertyDetailsOfValueBinding(uiComponent));
    }

    public void testDeactivateElResolverCustomConfig()
    {
        ELHelper elHelper = new CustomDefaultELHelper();
        // When deactivated, the getPropertyDetailsViaReflectionFallback method
        // is called which returns null in our custom version
        assertNull(elHelper.getPropertyDetailsOfValueBinding(uiComponent));
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(ExtValCoreConfigurationDeactivateElResolverTestCase.class);
    }

}
