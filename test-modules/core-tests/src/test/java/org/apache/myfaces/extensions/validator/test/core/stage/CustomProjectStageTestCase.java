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

import org.apache.myfaces.extensions.validator.core.JsfProjectStage;
import org.apache.myfaces.extensions.validator.core.ProjectStage;
import org.apache.myfaces.extensions.validator.test.core.AbstractExValCoreTestCase;
import org.junit.Assert;
import org.junit.Test;

public class CustomProjectStageTestCase extends AbstractExValCoreTestCase
{
    private static final String PROJECT_STAGE = "javax.faces.PROJECT_STAGE";
    
    @Test
    public void testDevelopmentStage()
    {
        servletContext.addInitParameter(PROJECT_STAGE, "Dev");
        Assert.assertTrue(CustomProjectStage.is(CustomProjectStage.Dev));
    }

    @Test
    public void testTestStage()
    {
        servletContext.addInitParameter(PROJECT_STAGE, "Test");
        Assert.assertTrue(CustomProjectStage.is(CustomProjectStage.Test));
    }

    @Test
    public void testProductionStage()
    {
        servletContext.addInitParameter(PROJECT_STAGE, "Prod");
        Assert.assertTrue(CustomProjectStage.is(CustomProjectStage.Prod));
    }

    @Test
    public void testDefaultStage()
    {
        Assert.assertTrue(ProjectStage.is(JsfProjectStage.Production.getValue()));
    }

    @Test
    public void testWrongDefaultStage1()
    {
        Assert.assertFalse(CustomProjectStage.is(CustomProjectStage.Dev));
    }

    @Test
    public void testWrongDefaultStage2()
    {
        Assert.assertFalse(CustomProjectStage.is(CustomProjectStage.Test));
    }
}