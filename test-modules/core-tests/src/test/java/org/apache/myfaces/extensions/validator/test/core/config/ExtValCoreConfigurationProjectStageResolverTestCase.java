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

import junit.framework.Test;

import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.JsfProjectStage;
import org.apache.myfaces.extensions.validator.core.ProjectStage;
import org.apache.myfaces.extensions.validator.core.ProjectStageResolver;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

/**
 * 
 * @author Rudy De Busscher
 * since v4
 *
 */
public class ExtValCoreConfigurationProjectStageResolverTestCase extends ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationProjectStageResolverTestCase(String name)
    {
        super(name);
    }

    public static class CustomProjectStageResolver implements ProjectStageResolver
    {

        public ProjectStage getCurrentProjectStage()
        {

            return ProjectStage.createStage(JsfProjectStage.UnitTest.getValue());
        }

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter("javax.faces.PROJECT_STAGE", "SystemTest");
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
                public ProjectStageResolver projectStageResolver()
                {
                    return new CustomProjectStageResolver();
                }

            };
        }
        else
        {
            return null;
        }
    }

    public void testProjectStageResolverDefault()
    {
        assertTrue(JsfProjectStage.is(JsfProjectStage.Production));
    }

    public void testProjectStageResolverWebXml()
    {
        assertTrue(JsfProjectStage.is(JsfProjectStage.SystemTest));
    }

    public void testProjectStageResolverCustomConfig()
    {
        assertTrue(JsfProjectStage.is(JsfProjectStage.UnitTest));
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(ExtValCoreConfigurationProjectStageResolverTestCase.class);
    }

}
