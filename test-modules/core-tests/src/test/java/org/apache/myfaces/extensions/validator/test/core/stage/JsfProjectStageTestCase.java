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
import org.apache.myfaces.extensions.validator.test.core.AbstractExValCoreTestCase;
import org.junit.Assert;
import org.junit.Test;

public class JsfProjectStageTestCase extends AbstractExValCoreTestCase
{
    private static final String PROJECT_STAGE = "javax.faces.PROJECT_STAGE";

    @Test
    public void testDevelopmentStage()
    {
        servletContext.addInitParameter(PROJECT_STAGE, "Development");
        Assert.assertTrue(JsfProjectStage.is(JsfProjectStage.Development));
    }

    @Test
    public void testUnitTestStage()
    {
        servletContext.addInitParameter(PROJECT_STAGE, "UnitTest");
        Assert.assertTrue(JsfProjectStage.is(JsfProjectStage.UnitTest));
    }

    @Test
    public void testSystemTestStage()
    {
        servletContext.addInitParameter(PROJECT_STAGE, "SystemTest");
        Assert.assertTrue(JsfProjectStage.is(JsfProjectStage.SystemTest));
    }

    @Test
    public void testProductionStage()
    {
        servletContext.addInitParameter(PROJECT_STAGE, "Production");
        Assert.assertTrue(JsfProjectStage.is(JsfProjectStage.Production));
    }

    @Test
    public void testDefaultStage()
    {
        Assert.assertTrue(JsfProjectStage.is(JsfProjectStage.Production));
    }

    @Test
    public void testWrongDefaultStage1()
    {
        Assert.assertFalse(JsfProjectStage.is(JsfProjectStage.Development));
    }

    @Test
    public void testWrongDefaultStage2()
    {
        Assert.assertFalse(JsfProjectStage.is(JsfProjectStage.UnitTest));
    }

    @Test
    public void testWrongDefaultStage3()
    {
        Assert.assertFalse(JsfProjectStage.is(JsfProjectStage.SystemTest));
    }
}