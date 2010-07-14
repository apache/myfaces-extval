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
package org.apache.myfaces.extensions.validator.test.core.stage;

import junit.framework.Test;
import junit.framework.TestSuite;
import org.apache.myfaces.extensions.validator.core.*;
import org.apache.myfaces.extensions.validator.test.core.AbstractExValCoreTestCase;

import javax.faces.context.FacesContext;

public class IndependentProjectStageTestCase extends AbstractExValCoreTestCase
{
    private static final String INDEPENDENT_PROJECT_STAGE = "custom.PROJECT_STAGE";
    private static final String JSF_PROJECT_STAGE = "javax.faces.PROJECT_STAGE";

    private static final String CUSTOM_DEV = "custom_dev";
    private static final String CUSTOM_PROD = "custom_prod";
    private static final String CUSTOM_TEST = "custom_test";

    public IndependentProjectStageTestCase(String name)
    {
        super(name);
    }

    public static Test suite()
    {
        return new TestSuite(IndependentProjectStageTestCase.class);
    }

    @Override
    protected void setUp() throws Exception
    {
        super.setUp();

        ExtValCoreConfiguration.use(new DefaultExtValCoreConfiguration()
        {
            @Override
            public ProjectStageResolver projectStageResolver()
            {
                return new DefaultProjectStageResolver()
                {
                    @Override
                    public ProjectStage getCurrentProjectStage()
                    {
                        String jsfProjectStageName = resolveProjectStageName("javax.faces.PROJECT_STAGE");
                        String independentProjectStageName = resolveProjectStageName(INDEPENDENT_PROJECT_STAGE);

                        if (!(jsfProjectStageName == null || "".equals(jsfProjectStageName)))
                        {
                            ProjectStageName result = ProjectStage.createStageName(jsfProjectStageName.trim());
                            for (JsfProjectStage jsfProjectStage : JsfProjectStage.values())
                            {
                                if (jsfProjectStage.getValue().equals(result))
                                {
                                    return ProjectStage.createStage(result);
                                }
                            }
                        }

                        if (!(independentProjectStageName == null || "".equals(independentProjectStageName)))
                        {
                            ProjectStageName independentResult = ProjectStage.createStageName(independentProjectStageName.trim());

                            //check jsf stage values first
                            ProjectStageName result = ProjectStage.createStageName(independentProjectStageName.trim());
                            for (JsfProjectStage jsfProjectStage : JsfProjectStage.values())
                            {
                                if (jsfProjectStage.getValue().equals(result))
                                {
                                    return ProjectStage.createStage(result);
                                }
                            }

                            //check custom stage values
                            if (ProjectStage.createStageName(CUSTOM_DEV).equals(independentResult) ||
                                    ProjectStage.createStageName(CUSTOM_TEST).equals(independentResult))
                            {
                                return ProjectStage.createStage(independentResult);
                            }
                        }

                        return createProjectStage(ProjectStage.createStageName(CUSTOM_PROD));
                    }
                };
            }
        }, true);
    }

    private String resolveProjectStageName(String parameterName)
    {
        try
        {
            return FacesContext.getCurrentInstance()
                    .getExternalContext().getInitParameter(parameterName);
        }
        catch (Exception e)
        {
            return null;
        }
    }

    public void testDevelopmentStage()
    {
        servletContext.addInitParameter(INDEPENDENT_PROJECT_STAGE, CUSTOM_DEV);
        assertTrue(ProjectStage.is(ProjectStage.createStageName(CUSTOM_DEV)));
    }

    public void testTestStage()
    {
        servletContext.addInitParameter(INDEPENDENT_PROJECT_STAGE, CUSTOM_TEST);
        assertTrue(ProjectStage.is(ProjectStage.createStageName(CUSTOM_TEST)));
    }

    public void testFallbackStage()
    {
        servletContext.addInitParameter(INDEPENDENT_PROJECT_STAGE, "SystemTest");
        assertTrue(JsfProjectStage.is(JsfProjectStage.SystemTest));
        assertTrue(ProjectStage.is(ProjectStage.createStageName("SystemTest")));
        assertTrue(ProjectStage.is(JsfProjectStage.SystemTest.getValue()));
    }

    public void testOverrideJsfStage()
    {
        servletContext.addInitParameter(INDEPENDENT_PROJECT_STAGE, CUSTOM_DEV);
        servletContext.addInitParameter(JSF_PROJECT_STAGE, "SystemTest");

        assertTrue(JsfProjectStage.is(JsfProjectStage.SystemTest));
        assertTrue(ProjectStage.is(JsfProjectStage.SystemTest.getValue()));
    }

    public void testProductionStage()
    {
        servletContext.addInitParameter(INDEPENDENT_PROJECT_STAGE, CUSTOM_PROD);
        assertTrue(ProjectStage.is(ProjectStage.createStageName(CUSTOM_PROD)));
    }

    public void testDefaultStage()
    {
        assertTrue(ProjectStage.is(ProjectStage.createStageName(CUSTOM_PROD)));
    }

    public void testWrongDefaultStage1()
    {
        assertFalse(ProjectStage.is(ProjectStage.createStageName(CUSTOM_DEV)));
    }

    public void testWrongDefaultStage2()
    {
        assertFalse(ProjectStage.is(ProjectStage.createStageName(CUSTOM_TEST)));
    }
}
