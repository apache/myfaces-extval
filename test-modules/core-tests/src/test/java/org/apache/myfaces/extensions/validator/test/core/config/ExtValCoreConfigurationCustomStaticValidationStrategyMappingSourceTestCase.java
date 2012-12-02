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
import javax.faces.context.FacesContext;

import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.validation.strategy.DefaultValidationStrategyFactory;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.junit.Assert;
import org.junit.Test;

/**
 * 
 * since v4
 *
 */
public class ExtValCoreConfigurationCustomStaticValidationStrategyMappingSourceTestCase extends
        ExtValCoreConfigurationTestCase
{

    public static class CustomValidationStrategy implements ValidationStrategy
    {

        public void validate(FacesContext facesContext, UIComponent uiComponent, MetaDataEntry metaDataEntry,
                Object convertedObject)
        {

        }

    }

    public static class Custom2ValidationStrategy implements ValidationStrategy
    {

        public void validate(FacesContext facesContext, UIComponent uiComponent, MetaDataEntry metaDataEntry,
                Object convertedObject)
        {

        }

    }

    @Override
    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(
                    ExtValInformation.WEBXML_PARAM_PREFIX + ".CUSTOM_STATIC_VALIDATION_STRATEGY_MAPPING",
                    "org.apache.myfaces.extensions.validator.core.config.ExtValCoreConfigurationCustomStaticValidationStrategyMappingSourceTestCaseWebXml");

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
                public String customStaticValidationStrategyMappingSource()
                {
                    return "org.apache.myfaces.extensions.validator.core.config.ExtValCoreConfigurationCustomStaticValidationStrategyMappingSourceTestCaseCustomConfig";
                }
            };
        }
        else
        {
            return null;
        }
    }

    @Test
    public void testCustomStaticValidationStrategyMappingSourceDefault()
    {
        DefaultValidationStrategyFactory validationStrategyFactory = new DefaultValidationStrategyFactory();
        // Something that isn't available, so should return null.
        Assert.assertNull(validationStrategyFactory.create("UnitTest"));
    }

    @Test
    public void testCustomStaticValidationStrategyMappingSourceWebXml()
    {
        DefaultValidationStrategyFactory validationStrategyFactory = new DefaultValidationStrategyFactory();
        Assert.assertEquals(CustomValidationStrategy.class.getName(), validationStrategyFactory.create("UnitTest").getClass()
                .getName());
    }

    @Test
    public void testCustomStaticValidationStrategyMappingSourceCustomConfig()
    {
        DefaultValidationStrategyFactory validationStrategyFactory = new DefaultValidationStrategyFactory();
        Assert.assertEquals(Custom2ValidationStrategy.class.getName(), validationStrategyFactory.create("UnitTest").getClass()
                .getName());
    }

}
