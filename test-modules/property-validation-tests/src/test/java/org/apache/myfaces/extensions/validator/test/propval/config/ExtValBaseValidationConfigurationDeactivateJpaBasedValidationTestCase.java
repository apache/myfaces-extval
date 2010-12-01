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
package org.apache.myfaces.extensions.validator.test.propval.config;

import java.util.List;

import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.baseval.DefaultExtValBaseValidationModuleConfiguration;
import org.apache.myfaces.extensions.validator.baseval.ExtValBaseValidationModuleConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfiguration;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfigurationNames;
import org.junit.Assert;
import org.junit.Test;


/**
 * 
 * @author Rudy De Busscher
 * since v4
 *
 */
public class ExtValBaseValidationConfigurationDeactivateJpaBasedValidationTestCase extends
        ExtValBaseValidationConfigurationTestCase
{


    @Override
    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".DEACTIVATE_JPA_BASED_VALIDATION", "true");

        }
    }

    @Override
    protected ExtValBaseValidationModuleConfiguration getCustomBaseValidationModuleConfiguration()
    {
        return new DefaultExtValBaseValidationModuleConfiguration()
        {

            @Override
            public boolean deactivateJpaBasedValidation()
            {
                return true;
            }

        };
    }

    @Test
    public void testExtValBaseValidationConfigurationDeactivateJpaBasedValidationDefault()
    {
        List<StaticConfiguration<String, String>> configs = ExtValContext.getContext().getStaticConfiguration(
                StaticConfigurationNames.META_DATA_TO_VALIDATION_STRATEGY_CONFIG);
        Assert.assertEquals(1, configs.size());
    }

    @Test
    public void testExtValBaseValidationConfigurationDeactivateJpaBasedValidationWebXml()
    {
        List<StaticConfiguration<String, String>> configs = ExtValContext.getContext().getStaticConfiguration(
                StaticConfigurationNames.META_DATA_TO_VALIDATION_STRATEGY_CONFIG);
        Assert.assertEquals(0, configs.size());
    }

    @Test
    public void testExtValBaseValidationConfigurationDeactivateJpaBasedValidationCustomConfig()
    {
        List<StaticConfiguration<String, String>> configs = ExtValContext.getContext().getStaticConfiguration(
                StaticConfigurationNames.META_DATA_TO_VALIDATION_STRATEGY_CONFIG);
        Assert.assertEquals(0, configs.size());
    }

}
