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
public class CustomProjectStageTestCase extends AbstractExValCoreTestCase
{
    private static final String PROJECT_STAGE = "javax.faces.PROJECT_STAGE";

    private final String paramValue;
    private final CustomProjectStage stage;

    public CustomProjectStageTestCase(String paramValue, CustomProjectStage stage)
    {
        this.paramValue = paramValue;
        this.stage = stage;
    }

    @Override
    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if(paramValue != null)
        {
            addInitParameter(PROJECT_STAGE, paramValue);
        }
    }

    @Test
    public void testStage()
    {
        Assume.assumeNotNull(paramValue);
        Assume.assumeNotNull(stage);
        Assert.assertTrue(CustomProjectStage.is(stage));
    }

    @Parameters
    public static Collection<Object[]> data()
    {
        // @formatter:off
        return Arrays.asList(new Object[][] {
            new Object[] { null, null },
            new Object[] { "Dev", CustomProjectStage.Dev },
            new Object[] { "Test", CustomProjectStage.Test },
            new Object[] { "Prod", CustomProjectStage.Prod }
        });
        // @formatter:on
    }

    @Test
    public void testDefaultStage()
    {
        Assume.assumeTrue(stage == null);
        Assert.assertTrue(ProjectStage.is(JsfProjectStage.Production.getValue()));
    }

    @Test
    public void testWrongDefaultStage1()
    {
        Assume.assumeTrue(stage == null);
        Assert.assertFalse(CustomProjectStage.is(CustomProjectStage.Dev));
    }

    @Test
    public void testWrongDefaultStage2()
    {
        Assume.assumeTrue(stage == null);
        Assert.assertFalse(CustomProjectStage.is(CustomProjectStage.Test));
    }
}