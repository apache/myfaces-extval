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
import junit.framework.Test;
import junit.framework.TestSuite;

public class CustomProjectStageTestCase extends AbstractExValCoreTestCase
{
    private static final String PROJECT_STAGE = "javax.faces.PROJECT_STAGE";
    
    public CustomProjectStageTestCase(String name)
    {
        super(name);
    }

    public static Test suite()
    {
        return new TestSuite(CustomProjectStageTestCase.class);
    }

    public void testDevelopmentStage()
    {
        servletContext.addInitParameter(PROJECT_STAGE, "Dev");
        assertTrue(CustomProjectStage.is(CustomProjectStage.Dev));
    }

    public void testTestStage()
    {
        servletContext.addInitParameter(PROJECT_STAGE, "Test");
        assertTrue(CustomProjectStage.is(CustomProjectStage.Test));
    }

    public void testProductionStage()
    {
        servletContext.addInitParameter(PROJECT_STAGE, "Prod");
        assertTrue(CustomProjectStage.is(CustomProjectStage.Prod));
    }

    public void testDefaultStage()
    {
        assertTrue(ProjectStage.is(JsfProjectStage.Production.getValue()));
    }

    public void testWrongDefaultStage1()
    {
        assertFalse(CustomProjectStage.is(CustomProjectStage.Dev));
    }

    public void testWrongDefaultStage2()
    {
        assertFalse(CustomProjectStage.is(CustomProjectStage.Test));
    }
}
