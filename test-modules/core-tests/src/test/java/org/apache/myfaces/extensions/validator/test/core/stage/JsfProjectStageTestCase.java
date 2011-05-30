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

import java.util.Arrays;
import java.util.Collection;

import org.apache.myfaces.extensions.validator.core.JsfProjectStage;
import org.apache.myfaces.extensions.validator.core.ProjectStage;
import org.apache.myfaces.extensions.validator.test.core.AbstractExValCoreTestCase;
import org.junit.Assert;
import org.junit.Assume;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

@RunWith(Parameterized.class)
public class JsfProjectStageTestCase extends AbstractExValCoreTestCase
{
    private static final String PROJECT_STAGE = "javax.faces.PROJECT_STAGE";

    private final String initParam;
    private final JsfProjectStage stage;

    public JsfProjectStageTestCase(String initParam, JsfProjectStage stage)
    {
        super();
        this.initParam = initParam;
        this.stage = stage;
    }

    @Parameters
    public static Collection<Object[]> data()
    {
        return Arrays.asList(new Object[][] { new Object[] { null, null },
            new Object[] { "Development", JsfProjectStage.Development },
            new Object[] { "UnitTest", JsfProjectStage.UnitTest },
            new Object[] { "SystemTest", JsfProjectStage.SystemTest },
            new Object[] { "Production", JsfProjectStage.Production }, });
    }

    @Override
    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if(initParam != null)
        {
            addInitParameter(PROJECT_STAGE, initParam);
        }
    }

    @Test
    public void testStage()
    {
        Assume.assumeNotNull(initParam);
        Assume.assumeNotNull(stage);
        Assert.assertTrue(JsfProjectStage.is(stage));
    }

    @Test
    public void testDefaultStage()
    {
        Assume.assumeTrue(stage == null);
        Assert.assertTrue(JsfProjectStage.is(JsfProjectStage.Production));
        Assert.assertTrue(ProjectStage.is(JsfProjectStage.Production.getValue()));
    }

    @Test
    public void testWrongDefaultStage1()
    {
        Assume.assumeTrue(stage == null);
        Assert.assertFalse(JsfProjectStage.is(JsfProjectStage.Development));
    }

    @Test
    public void testWrongDefaultStage2()
    {
        Assume.assumeTrue(stage == null);
        Assert.assertFalse(JsfProjectStage.is(JsfProjectStage.UnitTest));
    }

    @Test
    public void testWrongDefaultStage3()
    {
        Assume.assumeTrue(stage == null);
        Assert.assertFalse(JsfProjectStage.is(JsfProjectStage.SystemTest));
    }
}